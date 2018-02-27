## This is identical to the file in httppipe, except that we find the
## Adapter modules in different places (rather than coming from our
## own package, instead find them in the docker package itself.
##
## diff stevedore/inst/py/httppipe.py httppipe/inst/py/httppipe.py

import docker
import requests
import sys

IS_WINDOWS_PLATFORM = (sys.platform == 'win32')

if IS_WINDOWS_PLATFORM:
    from docker.transport import NpipeAdapter as HttpAdapter
else:
    from docker.transport import UnixAdapter as HttpAdapter

def string_is_binary(x):
  try:
    return x.find('\x00') > 0
  except TypeError:
    return False

# Start with the unix socket version because that's fairly easy to get
# going with and I can test it locally.  Then we can copy over all the
# bits for the windows support and test that locally there.
class Transporter(requests.Session):
    def __init__(self, base_url):
        super(Transporter, self).__init__()
        self.base_url = base_url
        self._custom_adapter = HttpAdapter(base_url)
        self.mount('http://', self._custom_adapter)
        self._unmount('https://')
        self.base_url = 'http://localhost'

    def _unmount(self, *args):
        for proto in args:
            self.adapters.pop(proto)

    def simple_request(self, verb, url, headers, data = None):
        res = self.request(method = verb, url = url, headers = headers,
                           data = data)
        headers = '\n'.join(
            ['{}: {}'.format(*i) for i in res.raw.headers.items()])
        content = res.content
        is_binary = string_is_binary(content)
        if is_binary:
            content = [ord(i) for i in content]
        elif type(content) == bytes:
            content = content.decode("UTF-8")
        return {'url': res.url,
                'status_code': res.status_code,
                'headers': headers,
                'is_binary': is_binary,
                'content': content}
