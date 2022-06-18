import re
import sys
import datetime

# - [ ] [YYYY-MM-DD[ HH:MM][ <[+[+]]nn uu[!|?]>]]text
regex: re.Pattern = re.compile(
    r"""^
        -\s\[\s\]\s # leading "- [ ] "
        (?P<datetime>
            (?P<year>\d\d\d\d)-
            (?P<month>\d\d)-
            (?P<day>\d\d)
            (?P<time>\s
                (?P<hour>\d\d):
                (?P<minute>\d\d)
            )?
            (?P<increment>
                \s<
                    ( (?P<fromtoday>\+)
                    | (?P<fromnow>\+\+)
                    )?
                    (?P<amount>\d+)\s
                    (?P<unit>days?|hours?|weeks?|months?|years?)
                    ( (?P<logtext>!)
                    | (?P<logquiet>\?)
                    )?
                >
            )?
        )?
        (?P<text>.*)
    $""",
    re.VERBOSE,
)

for line in sys.stdin:
    match = regex.match(line)
    if match:
        d = match.groupdict()
        text = d["text"]

        if not d["datetime"]:
            print(f"- [x] {text}")
            sys.exit(0)

        ts_prev = datetime.datetime(
            year=int(d.get("year")),
            month=int(d.get("month")),
            day=int(d.get("day")),
            hour=int(d.get("hour") or 0),
            minute=int(d.get("minute") or 0),
        )

        format_string = "%Y-%m-%d %H:%M" if d["time"] else "%Y-%m-%d"
        increment = d["increment"]
        if not increment:
            print(f"- [x] {ts_prev.strftime(format_string)}{text}")
            sys.exit(0)

        amount = int(d["amount"])
        unit = d["unit"]
        if unit == "day" or unit == "days":
            delta = datetime.timedelta(days=amount)
        elif unit == "hour" or unit == "hours":
            delta = datetime.timedelta(hours=amount)
        elif unit == "week" or unit == "weeks":
            delta = datetime.timedelta(weeks=amount)
        # TODO support years/months?
        else:
            exit(f"unrecognized unit: {unit}")
        now = datetime.datetime.now()
        if d["fromnow"]:
            ts_next = now + delta
        elif d["fromtoday"]:
            ts_next = (now + delta).replace(hour=ts_prev.hour, minute=ts_prev.minute)
        else:
            ts_next = ts_prev + delta
        print(f"- [ ] {ts_next.strftime(format_string)}{increment}{text}")
        if d["logtext"]:
            print(f"- [x] {ts_prev.strftime(format_string)}{text}")
        elif d["logquiet"]:
            print(f"- [x] {ts_prev.strftime(format_string)}")
