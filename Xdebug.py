import sublime
import sublime_plugin
import os
import socket
import base64
import threading
import types
import webbrowser
import xml.dom.minidom


session_active = False
xdebug_current = None
original_layout = None
protocol = None

buffers = {}  # Cache for editor views

is_debug_enabled = True


def _log(msg):
    if (is_debug_enabled):
        print '[Xdebug]: %s' % msg


def lookup_view(view):
    # Check to see if its a context view, stack view, inspect view, etc.
    if isinstance(view, str):
        if view in buffers:
            return buffers[view]

    # Check to see if the editor view has already been created
    if isinstance(view, XdebugView):
        return view

    # Try to resolve an xdebug view by name
    view_id = xid(view.name())
    if view_id in buffers:
        return buffers[view_id]

    # Is is a sublime view, if so create an editor view
    if isinstance(view, sublime.View):
        id = view.buffer_id()
        if id in buffers:
            buffers[id].view = view
        else:
            buffers[id] = XdebugView(view=view)

        return buffers[id]

    return None


def xid(name=None):
    '''
    Convenience method for generating a xdebug view id (xid) given a view name.
    It is assumed that the view name will start with "Xdebug"
    '''
    if name:
        return name.replace("Xdebug", "").rstrip().lstrip().lower()
    else:
        return None


def remote_debug_session(action='start'):
    '''
    Opens the browser and notifies Xdebug of start and stop commands
    '''
    global session_active

    url = get_setting('url')

    if url:
        if action == 'start':
            url += '?XDEBUG_SESSION_START=sublime.xdebug'
        elif action == 'stop':
            url += '?XDEBUG_SESSION_STOP=sublime.xdebug'
        else:
            url = None

        if url:
            webbrowser.open(url)

        session_active = (url and action == 'start')
    else:
        sublime.set_timeout(lambda: sublime.status_message('Xdebug: No URL defined in project settings file.'), 0)


def is_connected():
    global protocol
    return protocol and protocol.connected


def is_session_active():
    global session_active
    return session_active == True


def show_file(window, uri, lineno=None):
    '''
    Open or focus a window
    '''
    if window:
        window.focus_group(0)

    if sublime.platform() == 'windows':
        transport, filename = uri.split(':///', 1)  # scheme:///C:/path/file => scheme, C:/path/file
    else:
        transport, filename = uri.split('://', 1)  # scheme:///path/file => scheme, /path/file

    if transport == 'file' and os.path.exists(filename):
        window = sublime.active_window()
        if window.active_view().file_name() == filename:
            view = window.active_view()
        else:
            views = window.views()
            found = False
            for v in views:
                if v.file_name():
                    path = os.path.realpath(v.file_name())
                    if path == os.path.realpath(filename):
                        view = v
                        window.focus_view(v)
                        found = True
                        break
            if not found:
                view = window.open_file(filename)

        xview = lookup_view(view)
        if lineno != None:
            xview.current(int(lineno))

        return xview


def reset_current():
    '''
    Reset the current line marker
    '''
    global xdebug_current
    if xdebug_current:
        xdebug_current.erase_regions('xdebug_current_line')
        xdebug_current = None


def get_setting(key, default=None):
    try:
        settings = sublime.active_window().active_view().settings()
        if settings.has("xdebug"):
            xdebug = settings.get('xdebug')
            if xdebug and key in xdebug:
                return xdebug[key]
    except:
        pass
    return sublime.load_settings("SublimeXdebug.sublime-settings").get(key, default)


class DebuggerException(Exception):
    pass


class ProtocolException(DebuggerException):
    pass


class ProtocolConnectionException(ProtocolException):
    pass


class Protocol(object):
    '''
    Represents DBGp Protocol Language
    '''

    read_rate = 1024

    def __init__(self):
        self.stop()
        self.port = get_setting('port')
        self.listeners = {
            'start': [],
            'stop': []
        }

    def stop(self):
        self.buffer = ''
        self.connected = False
        self.listening = False
        self.server = None
        del self.transaction_id
        try:
            self.sock.close()
            for listener in self.listeners['stop']:
                listener()
        except:
            pass

        self.sock = None

    def transaction_id():
        '''
        The transaction_id property.
        '''

        def fget(self):
            self._transaction_id += 1
            return self._transaction_id

        def fset(self, value):
            self._transaction_id = value

        def fdel(self):
            self._transaction_id = 0
        return locals()

    transaction_id = property(**transaction_id())

    def read_until_null(self):
        if self.connected:
            if '\x00' in self.buffer:
                data, self.buffer = self.buffer.split('\x00', 1)
                return data
            else:
                self.buffer += self.sock.recv(self.read_rate)
                return self.read_until_null()
        else:
            raise ProtocolConnectionException("Xdebug is not connected")

    def read_data(self):
        length = self.read_until_null()
        message = self.read_until_null()
        if int(length) == len(message):
            return message
        else:
            raise ProtocolException("Length mismatch encountered while reading the xdebug message")

    def read(self):
        data = self.read_data()
        #print '<---', data
        document = xml.dom.minidom.parseString(data)
        return document

    def send(self, command, *args, **kwargs):
        if 'data' in kwargs:
            data = kwargs['data']
            del kwargs['data']
        else:
            data = None

        tid = self.transaction_id
        parts = [command, '-i %i' % tid]

        if args:
            parts.extend(args)
        if kwargs:
            parts.extend(['-%s %s' % pair for pair in kwargs.items()])
        parts = [part.strip() for part in parts if part.strip()]
        command = ' '.join(parts)
        if data:
            command += ' -- ' + base64.b64encode(data)

        try:
            self.sock.send(command + '\x00')
            #print '--->', command
        except Exception, x:
            raise ProtocolConnectionException(str(x))

    def accept(self):
        serv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        _log('Created server socket: ' + str(serv))

        if serv:
            try:
                sublime.set_timeout(lambda: sublime.status_message('Xdebug: Waiting for connection'), 0)
                serv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                serv.settimeout(1)
                serv.bind(('', self.port))
                serv.listen(1)

                self.listening = True
                self.sock = None

                _log('Server socket listening on port %s' % str(self.port))

            except Exception, x:
                sublime.set_timeout(lambda: sublime.status_message('Xdebug: Could not initialize port'), 0)
                raise ProtocolConnectionException('Could not initialize port ' + str(x))

            if self.listening:
                for listener in self.listeners['start']:
                    listener()

            while self.listening:
                _log('Listening for connection...')
                try:

                    self.sock, self.address = serv.accept()
                    _log('Connection accepted [%s, %s]' % (str(self.sock), str(self.address)))

                    sublime.set_timeout(lambda: sublime.status_message('Xdebug: Connected'), 0)
                    self.listening = False
                except socket.timeout:
                    pass

            if self.sock:
                self.connected = True
                self.sock.settimeout(None)
            else:
                self.connected = False
                self.listening = False

            try:
                serv.close()
                serv = None
            except:
                pass

            return self.sock
        else:
            raise ProtocolConnectionException('Could not create socket')

    def add_listener(self, on_type, fn):
        if fn:
            self.listeners[on_type].append(fn)

    def __str__(self):
        return '<Xdebug.Protocol {connected:%s, listening:%s, address:%s}' % (str(self.connected), str(self.listening), str(self.address))


class XdebugResponse():

    @staticmethod
    def get_root(doc):
        if isinstance(doc, xml.dom.minidom.Document):
            return doc.firstChild
        else:
            return doc

    @staticmethod
    def get_breakpoint_id(doc):
        return XdebugResponse.get_root(doc).getAttribute('id')

    @staticmethod
    def get_stack(doc):
        stack = []
        for node in XdebugResponse.get_root(doc).childNodes:
            if node.nodeName == 'stack':
                frame = XdebugStackElement()
                frame.where = node.getAttribute('where')
                frame.level = node.getAttribute('level')
                frame.type = node.getAttribute('type')
                frame.uri = node.getAttribute('filename')
                frame.lineno = node.getAttribute('lineno')

                stack.append(frame)

        return stack

    @staticmethod
    def get_doc_variables(doc):
        assert isinstance(doc, xml.dom.minidom.Document)
        xvars = []
        for child in doc.firstChild.childNodes:
            if child.nodeName == 'property':
                var = XdebugResponse.get_variables(child)
                xvars.append(var)
        return xvars

    @staticmethod
    def get_variables(node):
        if isinstance(node, xml.dom.minidom.Document):
            return XdebugResponse.get_doc_variables(node)
        else:
            assert isinstance(node, xml.dom.minidom.Element)

        var = XdebugVariable()
        var.name = unicode(node.getAttribute('name'))
        var.fullname = unicode(node.getAttribute('fullname'))
        var.type = unicode(node.getAttribute('type'))

        if node.hasAttribute('children') and int(node.getAttribute('children')):
            var.childCount = int(node.getAttribute('numchildren'))
            var.children = []

        if var.childCount > 0:
            for child in node.childNodes:
                if child.nodeName == 'property':
                    if not var.children:
                        var.children = []
                    childVar = XdebugResponse.get_variables(child)
                    var.children.append(childVar)
        else:
            try:
                var.value = unicode(' '.join(base64.b64decode(t.data) for t in node.childNodes if t.nodeType == t.TEXT_NODE or t.nodeType == t.CDATA_SECTION_NODE))
            except:
                var.value = unicode(' '.join(t.data for t in node.childNodes if t.nodeType == t.TEXT_NODE or t.nodeType == t.CDATA_SECTION_NODE))

        return var


class XdebugVariable(object):

    indent = '   '

    def __init__(self):
        self.name = None
        self.fullname = None
        self.type = None
        self.value = None
        self.children = None
        self.childCount = 0

    def __str__(self, level=0):
        out = unicode()

        if level > 0:
            out += (XdebugVariable.indent * level)

        if self.type == 'array':
            out += "%s = {%s} [%s]" % (self.name, self.type, self.childCount)
            if self.children:
                for child in self.children:
                    out += '\n'
                    out += child.__str__(level + 1)

        elif self.type == 'string':
            if self.value != None:
                out += "%s = '%s'" % (self.name, self.value)
            else:
                out += "%s = ''" % (self.name)

        elif self.type == 'uninitialized':
            out += "%s = uninitialized" % (self.name)

        else:
            out += "%s = '%s'" % (self.name, self.value)

        return out

    def __repr__(self, level=0):
        return self.__str__(level)


class XdebugStackElement(object):

    template = '{level:>3}: {type:<10} {where:<10} {uri}:{lineno}'

    def __init__(self):
        self.level = None
        self.type = None
        self.lineno = None
        self.uri = None
        self.where = None

    def __str__(self):
        return unicode(XdebugStackElement.template.format(level=self.level, type=self.type, where=self.where, lineno=self.lineno, uri=self.uri))


class XdebugBreakpoint(object):

    def __init__(self, uri, line):
        self.lineno = int(line)
        self.uri = uri
        self.id = None
        self.set = False
        self.add()

    def is_set(self):
        return self.set

    def add(self):
        if is_connected():
            protocol.send('breakpoint_set', t='line', f=self.uri, n=self.lineno)
            res = protocol.read()
            self.id = XdebugResponse.get_breakpoint_id(res)
            self.set = True if self.id else False
            #print 'Breakpoint set: %s' % self.__str__()
        return self.set

    def remove(self):
        if is_connected():
            protocol.send('breakpoint_remove', d=self.id)
            #print 'Breakpoint remove: %s' % self.__str__()
            return True
        else:
            return False

    def __str__(self):
        return "%s : [line:%d]" % (os.path.basename(self.uri), self.lineno)


class XdebugView(object):
    '''
    The XdebugView is a normal sublime view with some convenience methods.
    '''
    def __init__(self, view=None, name=None):
        self.view = view
        self.name = name
        self.is_open = False
        self.xid = xid(name)
        self.current_line = None
        self.auto_open = get_setting("%s_view_open" % self.xid, False)

    def __getattr__(self, attr):
        if hasattr(self.view, attr):
            return getattr(self.view, attr)
        if attr.startswith('on_'):
            return self
        raise(AttributeError, "Attribute does not exist: %s" % attr)

    def __call__(self, *args, **kwargs):
        pass

    def is_auto_open(self):
        return self.auto_open

    def open(self):
        if self.view == None or self.view.window() == None:
            self.create()

        self.is_open = True

    def close(self):
        print 'In close'
        print self.view
        if self.view is not None and isinstance(self.view, sublime.View):
            print 'doing this'
            sublime.active_window().focus_view(self.view)
            sublime.active_window().run_command("close")
            print 'view focused and closing'
            self.view = None
            self.is_open = False

    def create(self):
        # Check to see if the view is already opened... maybe left over from another session
        window = sublime.active_window()
        for view in window.views():
            if xid(view.name()) == self.xid:
                self.view = view
                break

        # Create view if not found
        if not self.view:
            self.view = window.new_file()

        self.view.set_scratch(True)
        self.view.set_name(self.name)
        self.view.settings().set('command_mode', False)
        self.view.settings().set('word_wrap', False)

        if self.name != None:
            self.view.set_name(self.name)

        index = get_setting("%s_view_index" % self.xid, 0)
        group = get_setting("%s_view_group" % self.xid, 0)

        window.set_view_index(self.view, group, index)
        #print 'View %s init at window location [group=%s, index=%s]' % (self.name, group, index)

    def center(self, lineno):
        line = self.lines(lineno)[0]
        self.view.show_at_center(line)

    def clear(self):
        self.view.set_read_only(False)
        e = self.view.begin_edit()
        self.view.erase(e, sublime.Region(0, self.view.size()))
        self.view.end_edit(e)
        self.view.set_read_only(True)

    def uri(self):
        return 'file://' + os.path.realpath(self.view.file_name())

    def lines(self, data=None):
        lines = []
        if data is None:
            regions = self.view.sel()
        else:
            if type(data) != types.ListType:
                data = [data]
            regions = []
            for item in data:
                if type(item) == types.IntType or item.isdigit():
                    regions.append(self.view.line(self.view.text_point(int(item) - 1, 0)))
                else:
                    regions.append(item)
        for region in regions:
            lines.extend(self.view.split_by_newlines(region))
        return [self.view.line(line) for line in lines]

    def rows(self, lines=None):
        if not lines:
            lines = self.lines()
        if not type(lines) == types.ListType:
            lines = [lines]
        return [self.view.rowcol(line.begin())[0] + 1 for line in lines]

    def append(self, content, edit=None, end=False):
        if not edit:
            edit = self.view.begin_edit()
            end = True
        self.view.insert(edit, self.view.size(), content + "\n")
        if end:
            self.view.end_edit(edit)
        return edit

    def on_load(self):
        if self.current_line:
            self.current(self.current_line)
            self.current_line = None

    def current(self, line):
        if self.is_loading():
            self.current_line = line
            return
        region = self.lines(line)
        self.add_regions('xdebug_current_line', region, 'xdebug.current_line', 'bookmark', sublime.HIDDEN)
        self.center(line)

    def show_quick_panel(self, command_map=None):
        if not command_map:
            command_map = {
                'Add/Remove Breakpoint': 'xdebug_breakpoint',
                'Clear all Breakpoints': 'xdebug_clear_all_breakpoints',
            }

            if is_connected():
                command_map.update({
                    #'Continue': 'xdebug_continue',
                    'Inspect': 'xdebug_inspect',
                    'Status': 'xdebug_status'
                })

            if protocol and protocol.listening or is_connected():
                command_map['Stop debugging'] = 'xdebug_stop_session'
            else:
                command_map['Start debugging'] = 'xdebug_listen'

        self.cmds = command_map
        self.items = command_map.keys()
        self.items.sort()

        self.view.window().show_quick_panel(self.items, self.on_quick_panel_select)

        return command_map

    def on_quick_panel_select(self, index):
        if index == -1:
            return

        command = self.cmds[self.items[index]]

        if isinstance(command, tuple):
            self.view.run_command(command[0], command[1])
        else:
            self.view.run_command(command)

    def update(self, raw, append=False):
        if not self.is_open:
            return

        if not append:
            self.clear()

        self.view.set_read_only(False)

        edit = self.view.begin_edit()

        if isinstance(raw, list):
            raw = '\n'.join(str(v) for v in raw)
        if isinstance(raw, XdebugVariable):
            raw = raw.__str__()

        if append:
            self.append(raw, edit, False)
        else:
            self.view.insert(edit, 0, raw)

        self.view.end_edit(edit)
        self.view.set_read_only(True)


class XdebugBreakpointView(XdebugView):

    def __init__(self, name=None):
        super(XdebugBreakpointView, self).__init__(name=name)
        self.breaks = {}
        if self.is_open:
            self.clear()

    def remove_all_breakpoints(self):
        if self.is_open:
            super(XdebugBreakpointView, self).clear()

        if self.breaks:
            for uri in self.breaks:
                for line in self.breaks[uri]:
                    self.breaks[uri][line].remove()

        self.breaks = {}
        self.update_view_markers()
        self.update()

    def on_connect(self):
        if self.has_breakpoints():
            for uri in self.breaks:
                for line in self.breaks[uri]:
                    self.breaks[uri][line].add()

    def add_breakpoint(self, uri, line):
        bp = XdebugBreakpoint(uri, line)
        if not uri in self.breaks:
            self.breaks[uri] = {}
        self.breaks[uri][line] = bp

    def remove_breakpoint(self, uri, line):
        bp = self.find_breakpoints(uri, line)
        if bp:
            bp.remove()
            del self.breaks[uri][line]

    def find_breakpoint_lines(self, uri):
        if uri in self.breaks:
            return self.breaks[uri].keys()

    def find_breakpoints(self, uri, line=None):
        if line == None and uri in self.breaks:
            return self.breaks[uri].values()
        elif line and line in self.breaks[uri]:
            return self.breaks[uri][line]
        else:
            return None

    def update_view_markers(self, view=None):
        if view == None:
            view = sublime.active_window().active_view()

        xview = lookup_view(view)
        found = self.find_breakpoint_lines(xview.uri())
        xview.erase_regions('dbgp_breakpoints')
        if found:
            xview.add_regions('dbgp_breakpoints', xview.lines(found), 'dbgp.breakpoint', 'dot', sublime.HIDDEN)

    def show_quick_panel(self):
        command_map = {}
        if self.has_breakpoints():
            for uri in self.breaks:
                for line in self.breaks[uri]:
                    bp = self.breaks[uri][line]
                    command_text = 'Open %s' % str(bp)
                    command_map[command_text] = ('xdebug_open_breakpoint', {'uri': uri, 'line': line})

        super(XdebugBreakpointView, self).show_quick_panel(command_map)

    def update(self):
        if self.is_open and self.breaks:
            out = []
            for uri in self.breaks:
                for line in self.breaks[uri]:
                    out.append(str(self.breaks[uri][line]))

            super(XdebugBreakpointView, self).update(out)

    def has_breakpoints(self, uri=None, line=None):
        if uri == None and line == None:
            return self.breaks and len(self.breaks) > 0
        elif line == None and uri in self.breaks:
            return len(self.breaks[uri]) > 0
        elif uri in self.breaks and line in self.breaks[uri]:
            return self.breaks[uri][line] != None
        else:
            return False


class XdebugVariableView(XdebugView):

    def __init__(self, name=None):
        super(XdebugVariableView, self).__init__(name=name)

    def create(self):
        super(XdebugVariableView, self).create()
        self.view.set_syntax_file("Packages/SublimeXdebug/Xdebug.tmLanguage")
        self.view.settings().set('fade_fold_buttons', False)

    def update(self, data=None, append=False):
        if isinstance(data, xml.dom.minidom.Document) or isinstance(data, xml.dom.minidom.Element):
            data = XdebugResponse.get_variables(data)

        super(XdebugVariableView, self).update(data, append)
        self.view.run_command("fold_all")


class XdebugInspectView(XdebugVariableView):

    def __init__(self, name):
        super(XdebugInspectView, self).__init__(name=name)
        self.inspections = []

    def refresh_inspections(self):
        if not self.is_open:
            return

        self.clear()

        tmp = self.inspections
        self.inspections = []

        for inspection in tmp:
            expr = inspection.name
            protocol.send('eval', data=expr)
            doc = protocol.read()
            self.update(doc, expr)

    def show_quick_panel(self):
        command_map = {
            'Clear Inspections': 'xdebug_clear_inspections'
        }
        super(XdebugInspectView, self).show_quick_panel(command_map)

    def update(self, data=None, expr=None):
        if isinstance(data, xml.dom.minidom.Document) or isinstance(data, xml.dom.minidom.Element):
            data = XdebugResponse.get_variables(data)
            data = data[0]
            data.name = expr

        self.inspections.append(data)

        # Append inspections
        super(XdebugInspectView, self).update(data, True)


class XdebugStackView(XdebugView):

    def __init__(self, name):
        super(XdebugStackView, self).__init__(name=name)
        self.stack = []

    def show_quick_panel(self):
        command_map = {}
        for stack in self.stack:
            command_text = 'Open stack %s : %s' % (stack.level, os.path.basename(stack.uri))
            command_map[command_text] = ('xdebug_open_stack_element', {'index': stack.level})

        super(XdebugStackView, self).show_quick_panel(command_map)

    def update(self, data=None):
        if isinstance(data, xml.dom.minidom.Document) or isinstance(data, xml.dom.minidom.Element):
            data = XdebugResponse.get_stack(data)
            self.stack = data

        super(XdebugStackView, self).update(data)


# Create single instance views
buffers['context'] = XdebugVariableView("Xdebug Context")
buffers['stack'] = XdebugStackView("Xdebug Stack")
buffers['inspect'] = XdebugInspectView("Xdebug Inspect")
buffers['breakpoints'] = XdebugBreakpointView("Xdebug Breakpoints")

'''
================================== SublimeXdebug Commands ==================================
'''


class XdebugListenCommand(sublime_plugin.TextCommand):
    '''
    Start listening for Xdebug connections
    '''
    def run(self, edit):
        global protocol, original_layout

        protocol = Protocol()
        # Add a listener on xdebug start, sublime.set_timeout brings executing of the method
        # back to thedisplay thread
        protocol.add_listener('start', lambda: sublime.set_timeout(self.connect_callback, 0))

        threading.Thread(target=self.thread_callback).start()

    def thread_callback(self):
        protocol.accept()
        if is_connected():
            sublime.set_timeout(self.gui_callback, 0)
        else:
            _log('#thread_callback: Xdebug is not connected')

    def connect_callback(self):
        global original_layout

        if not is_session_active():
            remote_debug_session('start')

            window = sublime.active_window()
            original_layout = window.get_layout()
            window.set_layout(get_setting('view_layout', {
                "cols": [0.0, 0.5, 1.0],
                "rows": [0.0, 0.7, 1.0],
                "cells": [
                    [0, 0, 2, 1],
                    [0, 1, 1, 2],
                    [1, 1, 2, 2]
                ]
            }))

            # Open all auto open views on xdebugger start
            for view in buffers.values():
                if view.xid and view.is_auto_open():
                    view.open()

    def gui_callback(self):
        # Dump the first xdebug message...
        protocol.read()
        # uri = init.getAttribute('fileuri')
        # show_file(self.view.window(), uri)

        bp_view = lookup_view('breakpoints')
        if bp_view and bp_view.has_breakpoints():
            bp_view.on_connect()

        self.view.run_command('xdebug_continue', {'state': 'run'})

    def is_enabled(self):
        if protocol:
            return False
        return True


class XdebugClearAllBreakpointsCommand(sublime_plugin.TextCommand):
    '''
    Clear all breakpoints
    '''
    def run(self, edit):
        bp_view = lookup_view('breakpoints')
        if bp_view:
            bp_view.remove_all_breakpoints()
        bp_view.update()


class XdebugBreakpointCommand(sublime_plugin.TextCommand):
    '''
    Toggle a breakpoint
    '''
    def run(self, edit):
        xview = lookup_view(self.view)
        bp_view = lookup_view('breakpoints')

        for row in xview.rows(xview.lines()):
            if bp_view.has_breakpoints(xview.uri(), row):
                bp_view.remove_breakpoint(xview.uri(), row)
            else:
                bp_view.add_breakpoint(xview.uri(), row)

        bp_view.update_view_markers(xview)
        bp_view.update()


class XdebugCommand(sublime_plugin.TextCommand):
    '''
    The Xdebug main quick panel menu
    '''
    def run(self, edit):
        xview = lookup_view(self.view)
        xview.show_quick_panel()


class XdebugContinueCommand(sublime_plugin.TextCommand):
    '''
    Continue execution menu and commands.

    This command shows the quick panel and executes the selected option.
    '''
    states = {
        'run': 'Run',
        'step_into': 'Step Into',
        'step_over': 'Step Over',
        'step_out': 'Step Out',
        'stop': 'Stop',
        'detach': 'Detach',
    }

    def run(self, edit, state=None):
        if not state or not state in self.states:
            self.view.window().show_quick_panel(self.states.values(), self.callback)
        else:
            self.callback(state)

    def callback(self, state):
        if state == -1:
            return
        if type(state) == int:
            state = self.states.keys()[state]

        _log('Here we go!')
        reset_current()

        protocol.send(state)
        res = protocol.read().firstChild

        for node in res.childNodes:
            if node.nodeName == 'xdebug:message':
                global xdebug_current
                filename = node.getAttribute('filename')
                lineno = int(node.getAttribute('lineno'))

                xdebug_current = show_file(self.view.window(), filename, lineno)
                sublime.set_timeout(lambda: sublime.status_message('Xdebug breakpoint: %s:%d ' % (filename, int(lineno))), 0)

        if (res.getAttribute('status') == 'break'):

            protocol.send('context_get')
            doc = protocol.read()
            lookup_view('context').update(doc)

            protocol.send('stack_get')
            doc = protocol.read()
            lookup_view('stack').update(doc)

            # update inspections if they exist
            lookup_view('inspect').refresh_inspections()

        if res.getAttribute('status') == 'stopping' or res.getAttribute('status') == 'stopped':
            self.view.run_command('xdebug_stop')
            self.view.run_command('xdebug_listen')
            sublime.set_timeout(lambda: sublime.status_message('Xdebug: Page finished executing. Reload to continue debugging.'), 0)

    def is_enabled(self):
        if is_connected():
            return True
        if protocol:
            sublime.set_timeout(lambda: sublime.status_message('Xdebug: Waiting for executing to start'), 0)
            return False
        sublime.set_timeout(lambda: sublime.status_message('Xdebug: Not running'), 0)
        return False


class XdebugStopCommand(sublime_plugin.TextCommand):
    '''
    Close the socket and stop listening to xdebug
    '''
    def run(self, edit):
        global protocol
        global original_layout
        try:
            protocol.stop()
            reset_current()

            lookup_view('context').clear()
            lookup_view('stack').clear()

            _log('Xdebug socket connection terminated')
        except:
            pass

        finally:
            protocol = None

    def is_enabled(self):
        if protocol:
            return True
        return False


class XdebugStopSessionCommand(sublime_plugin.TextCommand):
    '''
    Run the xdebug_stop command to close the current connection then also initiate a
    session stop. when the session stops, the layout should be set back to the origional
    layout, and the xdebug session stop command sent to the server
    '''
    def run(self, edit):
        global original_layout

        self.view.run_command('xdebug_stop')

        try:
            remote_debug_session('stop')
            for view in buffers:
                if buffers[view].xid:
                    _log('Closing view %s' % view)
                    buffers[view].close()

            sublime.active_window().set_layout(original_layout)
        except:
            pass

    def is_enabled(self):
        return True


class XdebugStatus(sublime_plugin.TextCommand):
    '''
    DBGp status command
    '''
    def run(self, edit):
        protocol.send('status')
        res = protocol.read().firstChild
        sublime.status_message(res.getAttribute('reason') + ': ' + res.getAttribute('status'))

    def is_enabled(self):
        return is_connected()


class EventListener(sublime_plugin.EventListener):

    def on_query_context(self, view, key, operator, operand, match_all):
        if key == "xdebug_running":
            return is_connected() == operand
        return None

    def on_load(self, view):
        lookup_view(view).on_load()


class XdebugOpenStackElement(sublime_plugin.TextCommand):

    def run(self, edit, index=0):
        xview = lookup_view(self.view)
        index = int(index)
        if xview.stack[index]:
            stack = xview.stack[index]
            if stack:
                show_file(self.view.window(), stack.uri, stack.lineno)

    def is_enabled(self):
        return lookup_view('stack').is_open

    def is_visible(self):
        return lookup_view('stack').is_open


class XdebugOpenBreakpoint(sublime_plugin.TextCommand):

    def run(self, edit, uri=None, line=None):
        if not uri and not line:
            return

        xview = lookup_view(self.view)
        bp = xview.breaks[uri][line]
        if bp:
            show_file(self.view.window(), bp.uri, bp.lineno)

    def is_enabled(self):
        return lookup_view('breakpoints').is_open

    def is_visible(self):
        return lookup_view('breakpoints').is_open


'''
class XdebugDoubleClick(sublime_plugin.TextCommand):
    def run(self, edit):
        xview = lookup_view(self.view)

        def get_first_row():
            rows = xview.rows()
            if len(rows) > 0:
                return rows[0]

        if xview.xid == 'breakpoints':
            row = get_first_row()
            bp = xview.breaks[row - 1]
            if bp:
                show_file(self.view.window(), bp.uri)

        elif xview.xid == 'stack':
            row = get_first_row()
            stack = xview.stack[row - 1]
            if stack:
                show_file(self.view.window(), stack.uri, stack.lineno)

    def is_enabled(self):
        return is_connected()
'''


class XdebugInspectCommand(sublime_plugin.TextCommand):
    def run(self, edit):

        sel = ''
        for region in self.view.sel():
            if not region.empty():
                sel = self.view.substr(region)

        self.view.window().show_input_panel('Xdebug Inspect',
            sel, self.on_done, self.on_change, self.on_cancel)

    def is_enabled(self):
        return is_connected()

    def on_done(self, expr):
        protocol.send('eval', data=expr)
        doc = protocol.read()

        inspect_view = lookup_view('inspect')
        if not inspect_view.is_open:
            inspect_view.open()

        inspect_view.update(doc, expr)

    def on_change(self, line):
        pass

    def on_cancel(self):
        pass


class XdebugClearInspectionsCommand(sublime_plugin.TextCommand):

    def run(self, edit):
        inspect_view = lookup_view('inspect')
        inspect_view.clear()

    def is_enabled(self):
        return lookup_view('inspect').is_open


class XdebugOpenBreakpointView(sublime_plugin.WindowCommand):
    def run(self):
        bp_view = lookup_view('breakpoints')
        bp_view.open()
        bp_view.update()

    def is_enabled(self):
        return not lookup_view('breakpoints').is_open

    def is_visible(self):
        return not lookup_view('breakpoints').is_open
