import asyncio
import shutil
import os

# Approximately max minutes * 6
_MAX_CHECKS = 600
_TESTING = False

_PROGRESS_FILE = 'progress.vtp'


# This allows a development version of a code, say, to run, produce results and
# have those results adopted by the stable version. This is normally indicated by
# use of a parameter DEVELOPMENT=1. This allows the stable version to take care
# of all normal runs except those for while an experimental set-up is requested.
# However, this is very preliminary
# functionality, tested in very specific situations
@asyncio.coroutine
def observe(guid, transferrer, update_callback):
    transferrer.connect()

    completed = None

    checks = 0
    target = {_PROGRESS_FILE: '%s/progress.vtp' % guid.lower()}
    target_dir = '/tmp/obs-%s' % guid
    try:
        shutil.rmtree(target_dir)
    except:
        pass
    os.mkdir(target_dir)
    while checks < _MAX_CHECKS:
        checks += 1
        yield from asyncio.sleep(10)
        try:
            if not _TESTING:
                transferrer.pull_files(target, target_dir, '.')
            else:
                shutil.copyfile('/tmp/%s.vtp' % guid.lower(), os.path.join(target_dir, _PROGRESS_FILE))
        except Exception as e:
            print(e)
            pass
        else:
            with open(os.path.join(target_dir, _PROGRESS_FILE), 'r') as f:
                lines = f.read().split('\n')
                if lines[1] == 'SUCCESS':
                    completed = True
                elif lines[1] == 'IN_PROGRESS':
                    update_callback(float(lines[2]) if len(lines[2]) > 0 else None, "\n".join(lines[3:]))
                else:
                    completed = "%s\n%s" % (lines[1], "\n".join(lines[3:]))
            os.unlink(os.path.join(target_dir, _PROGRESS_FILE))
            if completed is not None:
                break

    transferrer.disconnect()

    if completed is not True:
        raise RuntimeError(completed)

    return completed
