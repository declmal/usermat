import logging
import time
from functools import wraps


#   int values: 0(None) by default, 10(DEBUG),
#     20(INFO), 30(WARNING), 40(ERROR), 50(CRITICAL).

class ColoredFormatter(logging.Formatter):
    def __init__(self, fmt=None, datefmt=None, style='%'):
        super(ColoredFormatter, self).__init__(fmt, datefmt, style)

        self.log_colors = {
            "DEBUG": "\033[38;5;111m",
            "INFO": "\033[38;5;47m",
            "WARNING": "\033[38;5;178m",
            "ERROR": "\033[38;5;196m",
            "CRITICAL": "\033[30;48;5;196m",
            "DEFAULT": "\033[38;5;15m",
            "RESET": "\033[0m"
        }

    def format(self, record):
        log_color = self.get_color(record.levelname)
        message = super(ColoredFormatter, self).format(record)
        message = log_color + message + self.log_colors["RESET"]
        return message

    def get_color(self, level_name):
        lname = level_name if level_name in self.log_colors else "DEFAULT"
        return self.log_colors[lname]


class FilterList(logging.Filter):
    """ Filter with logging module

        Filter rules as below:
            {allow|disable log name} > level no > keywords >
            {inheritance from parent log name} > by default filter
        TODO:
    """
    def __init__(self, default=False, allows=[], disables=[],
            keywords=[], log_level=logging.INFO):
        self.rules = {}
        self._internal_filter_rule = "_internal_filter_rule"
        self.log_level = log_level
        self.keywords = keywords

        self.rules[self._internal_filter_rule] = default
        for name in allows:
            splits = name.split(".")
            rules = self.rules
            for split in splits:
                if split not in rules:
                    rules[split] = {}
                rules = rules[split]

            rules[self._internal_filter_rule] = True

        for name in disables:
            splits = name.split(".")
            rules = self.rules
            for split in splits:
                if split not in rules:
                    rules[split] = {}
                rules = rules[split]

            rules[self._internal_filter_rule] = False

    def filter(self, record):
        rules = self.rules
        rv = rules[self._internal_filter_rule]

        splits = record.name.split(".")
        for split in splits:
            if split in rules:
                rules = rules[split]
                if self._internal_filter_rule in rules:
                    rv = rules[self._internal_filter_rule]
            else:
                if record.levelno >= self.log_level:
                    return True

                for keyword in self.keywords:
                    if keyword in record.getMessage():
                        return True

                return rv

        return rv

def log_init(level=logging.NOTSET):
    logging.basicConfig(level=level)
    formatter = ColoredFormatter(
            fmt="[ %(asctime)s %(name)s.%(levelname)s ] %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S")

    log_filter = FilterList(
                log_level=logging.DEBUG,
                default=False)
    for handler in logging.root.handlers:
        handler.addFilter(log_filter)
        handler.setFormatter(formatter)

def timethis(func):
    @wraps(func)
    def _wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        logger = logging.getLogger("frt.timing")
        logger.warning(
            "Function (%s) has been executed for (%s) seconds",
            func.__name__, end-start)
        return result
    return _wrapper
