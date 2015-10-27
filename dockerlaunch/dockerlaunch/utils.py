import os

# Horrendous hack to work around bug in Python <=3.4.0


def _find_urandom_fd():
    fds = dict([(os.path.realpath(os.path.join('/proc', 'self', 'fd', p)), p) for p in os.listdir('/proc/self/fd')])
    return int(fds['/dev/urandom'])
