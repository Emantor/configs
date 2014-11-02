#!/usr/bin/perl
my $blanked = 0;
open (IN, "xscreensaver-command -watch |");
open MPVFIFO, ">/home/phoenix/.mpv/fifo" or die $!;
while (<IN>) {
    if (m/^(BLANK|LOCK)/) {
        if (!$blanked) {
            print MPVFIFO "set pause yes\n";
            system "killall -SIGUSR1 dunst";
            $blanked = 1;
        }
    } elsif (m/^UNBLANK/) {
        system "killall -SIGUSR2 dunst";
        $blanked = 0;
    }
}

