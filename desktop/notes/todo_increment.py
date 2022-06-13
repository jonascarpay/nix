import re
import sys
import datetime

regex: re.Pattern = re.compile(
    r"^- \[ \] (?P<year>\d\d\d\d)-(?P<month>\d\d)-(?P<day>\d\d).(?P<hour>\d\d):(?P<minute>\d\d) <(?P<increment>\d+) (?P<unit>days?|hours?|weeks?|months?|years?)>(?P<text>.*)$"
)

# TODO flags
# don't print previous
# don't print text
# relative to now
for line in sys.stdin:
    match = regex.match(line)
    if match:
        d = match.groupdict()
        ts_prev = datetime.datetime(
            year=int(d.get("year")),
            month=int(d.get("month")),
            day=int(d.get("day")),
            hour=int(d.get("hour")),
            minute=int(d.get("minute")),
        )
        increment = int(d["increment"])
        unit = d["unit"]
        if unit == "day" or unit == "days":
            delta = datetime.timedelta(days=increment)
        elif unit == "hour" or unit == "hours":
            delta = datetime.timedelta(hours=increment)
        elif unit == "week" or unit == "weeks":
            delta = datetime.timedelta(weeks=increment)
        # TODO support years/months
        # elif unit == "month" or unit == "months":
        #     delta = datetime.timedelta(months=increment)
        # elif unit == "year" or unit == "years":
        #     delta = datetime.timedelta(years=increment)
        else:
            exit(f"unrecognized unit: {unit}")
        format_string = "%Y-%m-%d %H:%M"
        ts_next = ts_prev + delta
        text = d["text"]
        print(f"- [ ] {ts_next.strftime(format_string)} <{increment} {unit}>{text}")
        print(f"- [x] {ts_prev.strftime(format_string)}{text}")
