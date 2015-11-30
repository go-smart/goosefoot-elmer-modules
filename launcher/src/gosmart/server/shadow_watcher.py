import asyncio
import os

# Approximately max minutes * 6
_MAX_CHECKS = 600


@asyncio.coroutine
def observe(guid, transferrer):
    transferrer.connect()

    completed = False

    checks = 0
    progress_file = 'progress.txt'
    target = {progress_file: '%s/progress.txt' % guid.lower()}
    target_dir = '/tmp/obs-%s' % guid
    while checks < _MAX_CHECKS:
        checks += 1
        yield from asyncio.sleep(10)
        try:
            transferrer.pull_files(target, target_dir, '.')
        except Exception as e:
            print(e)
            pass
        else:
            with open(os.path.join(target_dir, progress_file), 'r') as f:
                completed = f.read() == guid.upper()
            break

    transferrer.disconnect()

    return completed
