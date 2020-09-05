#!/bin/sh
# strftime = format date and time

Date()
{
    date "+(365/%j) %F | %B %d, %A"
}

Clock()
{
    date "+%R"
}

Uptime()
{
    uptime | sed -e 's/^.*up/up/' -e 's/[0-9] user.* l/l/' -e '/ /s/  */ /g'
}

echo -n | dmenu -nb "#000000" -nf "#000000" -sb "#000000" -p "$(Date) | $(Clock) | $(Uptime)"
