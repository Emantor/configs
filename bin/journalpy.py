#!/usr/bin/env python3
import select
import systemd.journal
import notify2
from time import sleep

notify2.init('Journal')

j = systemd.journal.Reader()
j.seek_tail()
# Workaround bug #979487 (https://bugzilla.redhat.com/show_bug.cgi?id=979487)
j.get_next(-1)

while True:
    entry = j.get_next()
    if entry:
        n = notify2.Notification("Journal",str(entry['MESSAGE']),"")
        n.show()
    j.wait()
