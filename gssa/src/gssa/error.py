from enum import Enum


class Error(Enum):
    SUCCESS, E_UNKNOWN, E_CLIENT, E_SERVER, E_MODEL, IN_PROGRESS = range(6)


def makeError(ref, message):
    id = ref.value if isinstance(ref, Error) else Error[ref].value
    code = ref.name if isinstance(ref, Error) else Error[ref].name

    return {'id': id, 'code': code, 'message': message}
