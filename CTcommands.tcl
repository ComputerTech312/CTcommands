#################################################################################################################################################################
# CTcommands.tcl
#################################################################################################################################################################
#Author    ComputerTech
#IRC       Irc.DareNet.Org  #ComputerTech
#Email     ComputerTech@DareNet.Org
#GitHub    https://github.com/computertech312
#Version   0.3
#Released  09/03/2021
#################################################################################################################################################################
# Description:
#
#               A Elaborate Public and Message
#               Command Script.
#
# Credits:
#
#               Special thanks to Suntop,Spyda,BdS,CrazyCat
#               and Sir_Fz for all the help.
#
# History: 
#
#               - 0.3: Added matchattr check to prevent punishment of 
#                      bot users. (thanks to m4s for suggesting this)
#               - 0.2: Improved some code.
#               - 0.1: First release.
#               
#################################################################################################################################################################
# Start Of Configuration #
##########################
# Set trigger of the commands.
##
set ctcmd(trig) "!"

##################
# Set to use Auth command or not (WARNING, disabling this may cause vulnerabilities)
##
# On  = 1
# Off = 0
##
set ctcmd(authcheck) "1"

###################
# Set flag for Commands.
##
# Owner     = n
# Master    = m
# Op        = o
# Voice     = v
# Friend    = f
# Everyone  = -
##
set ctcmd(flag) "omn"

##################
# Type of IRCd Your Network Uses
##
# 1 = Unrealircd
# 2 = Inspircd
# 3 = Freenode
# 4 = IRCnet
# 5 = Other
##
set ctcmd(ircd) "1"

##################
# Set to use Notice Or Privmsg for Output of Commands
##
#1 = Notice
#2 = Privmsg
##
set ctcmd(msg) "1"

##################
# Set the default ban time, if no Ban time is specified (in minutes 0 means No Expiry)
##
set ctcmd(bant) "60"

##################
# Set Ban hostmask Type
##
# 0 *!user@host
# 1 *!*user@host
# 2 *!*@host
# 3 *!*user@*.host
# 4 *!*@*.host
# 5 nick!user@host
# 6 nick!*user@host
# 7 nick!*@host
# 8 nick!*user@*.host
# 9 nick!*@*.host
##
set ctcmd(btype) "2"

########################
# End Of Configuration #
#################################################################################################################################################################
namespace eval ctcommands {
set ctcmd(author) "ComputerTech"
set ctcmd(ver) "0.3"
set ctcmd(logo) "\00302CTcommands\002"

bind PUB $ctcmd(flag) $ctcmd(trig)cmdhelp ctcommands::pub:cmdhelp
bind PUB $ctcmd(flag) $ctcmd(trig)version ctcommands::pub:version
bind PUB $ctcmd(flag) $ctcmd(trig)owner ctcommands::pub:owner
bind PUB $ctcmd(flag) $ctcmd(trig)deowner ctcommands::pub:deowner
bind PUB $ctcmd(flag) $ctcmd(trig)protect ctcommands::pub:protect
bind PUB $ctcmd(flag) $ctcmd(trig)deprotect ctcommands::pub:deprotect
bind PUB $ctcmd(flag) $ctcmd(trig)op ctcommands::pub:op
bind PUB $ctcmd(flag) $ctcmd(trig)deop ctcommands::pub:deop
bind PUB $ctcmd(flag) $ctcmd(trig)halfop ctcommands::pub:halfop
bind PUB $ctcmd(flag) $ctcmd(trig)dehalfop ctcommands::pub:dehalfop
bind PUB $ctcmd(flag) $ctcmd(trig)voice ctcommands::pub:voice
bind PUB $ctcmd(flag) $ctcmd(trig)devoice ctcommands::pub:devoice
bind PUB $ctcmd(flag) $ctcmd(trig)say ctcommands::pub:say
bind PUB $ctcmd(flag) $ctcmd(trig)rehash ctcommands::pub:rehash
bind PUB $ctcmd(flag) $ctcmd(trig)restart ctcommands::pub:restart
bind PUB $ctcmd(flag) $ctcmd(trig)die ctcommands::pub:die
bind PUB $ctcmd(flag) $ctcmd(trig)topic ctcommands::pub:topic
bind PUB $ctcmd(flag) $ctcmd(trig)chanset ctcommands::pub:chanset
bind PUB $ctcmd(flag) $ctcmd(trig)ban ctcommands::pub:ban
bind PUB $ctcmd(flag) $ctcmd(trig)unban ctcommands::pub:unban
bind PUB $ctcmd(flag) $ctcmd(trig)kick ctcommands::pub:kick
bind PUB $ctcmd(flag) $ctcmd(trig)kickban ctcommands::pub:kickban
bind PUB $ctcmd(flag) $ctcmd(trig)mode ctcommands::pub:mode
bind PUB $ctcmd(flag) $ctcmd(trig)join ctcommands::pub:join
bind PUB $ctcmd(flag) $ctcmd(trig)part ctcommands::pub:part
bind PUB $ctcmd(flag) $ctcmd(trig)cycle ctcommands::pub:cycle
bind PUB $ctcmd(flag) $ctcmd(trig)invite ctcommands::pub:invite
bind PUB $ctcmd(flag) $ctcmd(trig)mute ctcommands::pub:mute
bind PUB $ctcmd(flag) $ctcmd(trig)unmute ctcommands::pub:unmute
bind PUB $ctcmd(flag) $ctcmd(trig)ping ctcommands::pub:ping
bind PUB $ctcmd(flag) $ctcmd(trig)act ctcommands::pub:act
bind PUB $ctcmd(flag) $ctcmd(trig)ignores ctcommands::pub:ignores
bind PUB $ctcmd(flag) $ctcmd(trig)addignore ctcommands::pub:addignore
bind PUB $ctcmd(flag) $ctcmd(trig)delignore ctcommands::pub:delignore
bind PUB $ctcmd(flag) $ctcmd(trig)bans ctcommands::pub:bans
bind PUB $ctcmd(flag) $ctcmd(trig)addban ctcommands::pub:addban
bind PUB $ctcmd(flag) $ctcmd(trig)delban ctcommands::pub:delban
bind PUB $ctcmd(flag) $ctcmd(trig)adduser ctcommands::pub:adduser
bind PUB $ctcmd(flag) $ctcmd(trig)deluser ctcommands::pub:deluser
bind PUB $ctcmd(flag) $ctcmd(trig)chattr ctcommands::pub:chattr
bind PUB $ctcmd(flag) $ctcmd(trig)addhost ctcommands::pub:addhost
bind PUB $ctcmd(flag) $ctcmd(trig)delhost ctcommands::pub:delhost
bind PUB $ctcmd(flag) $ctcmd(trig)whois ctcommands::pub:whois
bind MSG - auth ctcommands::auth
bind MSG - deauth ctcommands::deauth

proc ct:set:gl {nick chan} {
global ctcmd
	if {$ctcmd(msg) == "1"} {
		set ctcmd(output) "NOTICE $nick"
	} else {
		set ctcmd(output) "PRIVMSG $nick"
	}
	if {$ctcmd(msg) == "1"} {
		set ctcmd(out) "NOTICE $nick"
	} else {
		set ctcmd(out) "PRIVMSG $chan"
	}
}
proc auth {nick host hand text} {
    global botnick ctcmd
    set pw [lindex [split $text] 0]
    if {$pw == ""} {
        puthelp "$ctcmd(output) :Syntax: /msg $botnick auth <password>"
        return }
    if {[passwdok $hand ""]} {
        putserv "$ctcmd(output) :You haven't set your password. Use: \[/msg $botnick pass <password>\] to set a password"
        return }
    if {[matchattr $hand Q]} {
        putserv "$ctcmd(output) :You have authenticated already"
        return }
    if {![passwdok $hand $pw]} {
        putserv "$ctcmd(output) :Authentication Rejected, Incorrect Password"
        return }
    chattr $hand +Q
    putserv "$ctcmd(output) :Authentication Accepted"
    return }
proc deauth {nick host hand text} {
    global botnick ctcmd
    set pw [lindex [split $text] 0]
    if {$pw == ""} {
        puthelp "$ctcmd(output) :Syntax: /msg $botnick deauth <password>"
        return }
    if {[passwdok $hand ""]} {
        puthelp "$ctcmd(output) :You haven't a Password set. Use: /msg $botnick pass <password>"
        return }
    if {![matchattr $hand Q]} {
        puthelp "$ctcmd(output) :You are not Authenticated"
        return }
    if {![passwdok $hand $pw]} {
        puthelp "$ctcmd(output) :Deauthentication Rejected, Invalid Password"
        return }
    chattr $hand -Q
    putserv "$ctcmd(output) :Deauthentication Successful"
    return
}
proc check:auth {nick hand} {
    global ctcmd botnick
    if {$ctcmd(authcheck) == "0" } {
        return 1
    }
    if {[matchattr $hand Q]} {
        return 1
    } else {
        puthelp "$ctcmd(output) :You are not authenticated, /msg $botnick auth <pass>"
        return 0
    }
}
proc pub:cmdhelp {nick host hand chan text} {
  global ctcmd botnick
if {![check:auth $nick $hand]} {return 0}
   putserv "$ctcmd(out) :$ctcmd(logo) \[Public Commands]\]"
   putserv "$ctcmd(out) :< > means required. \[ \] means not required"
   putserv "$ctcmd(out) :"
   putserv "$ctcmd(out) :$ctcmd(trig)voice \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)devoice \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)op \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)deop \[nicks\]"
if {$ctcmd(ircd) < "3"} {
   putserv "$ctcmd(out) :$ctcmd(trig)halfop \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)dehalfop \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)protect \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)deprotect \[nicks\]"
   putserv "$ctcmd(out) :$ctcmd(trig)owner <nicks>"
   putserv "$ctcmd(out) :$ctcmd(trig)deowner <nicks>"
   }
   putserv "$ctcmd(out) :$ctcmd(trig)kick <nicks>"
   putserv "$ctcmd(out) :$ctcmd(trig)ban <nicks/host>"
   putserv "$ctcmd(out) :$ctcmd(trig)unban <nicks/mask>"
   putserv "$ctcmd(out) :$ctcmd(trig)kickban <nicks>"
if {$ctcmd(ircd) < "3"} {
	 putserv "$ctcmd(out) :$ctcmd(trig)mute <nicks>"
	 putserv "$ctcmd(out) :$ctcmd(trig)unmute <nicks>"
   }
   putserv "$ctcmd(out) :$ctcmd(trig)say \[text\]"
   putserv "$ctcmd(out) :$ctcmd(trig)act \[text\]"
   putserv "$ctcmd(out) :$ctcmd(trig)topic \[text\]"
   putserv "$ctcmd(out) :$ctcmd(trig)mode ≤modes>"
   putserv "$ctcmd(out) :$ctcmd(trig)invite <nick>"
   putserv "$ctcmd(out) :$ctcmd(trig)join <channel>"
   putserv "$ctcmd(out) :$ctcmd(trig)part \[channel\]"
   putserv "$ctcmd(out) :$ctcmd(trig)cycle \[channel\]"
   putserv "$ctcmd(out) :$ctcmd(trig)adduser <nick>"
   putserv "$ctcmd(out) :$ctcmd(trig)deluser <nick"
   putserv "$ctcmd(out) :$ctcmd(trig)addhost <nick> <host>"
   putserv "$ctcmd(out) :$ctcmd(trig)delhost <nick> <host>"
   putserv "$ctcmd(out) :$ctcmd(trig)chattr <nick> <+/-flags> \[channel\]"
   putserv "$ctcmd(out) :$ctcmd(trig)Addban <nick/host> \[reason\] \[time\]"
   putserv "$ctcmd(out) :$ctcmd(trig)delban <nick/host>"
   putserv "$ctcmd(out) :$ctcmd(trig)banlist \[channel\]"
   putserv "$ctcmd(out) :$ctcmd(trig)addignore <nick/host> \[time\] \[reason\]"
   putserv "$ctcmd(out) :$ctcmd(trig)delignore <nick/host>"
   putserv "$ctcmd(out) :$ctcmd(trig)ignores"
   putserv "$ctcmd(out) :$ctcmd(trig)whois <nick>"
if {[matchattr [nick2hand $nick] $ctcmd(flag))]} {
   putserv "$ctcmd(out) :$ctcmd(trig)rehash"
   putserv "$ctcmd(out) :$ctcmd(trig)restart \[reason\]"
   putserv "$ctcmd(out) :$ctcmd(trig)die \[reason\]"
}
   putserv "$ctcmd(out) :\[Auth commands\]"
   putserv "$ctcmd(out) :/msg ${botnick} auth <password> :Authentication Command"
   putserv "$ctcmd(out) :/msg ${botnick} deauth <password>  :Deauthentication Command"
   putserv "$ctcmd(out) :/msg ${botnick} pass <password>  :Set Password Command"
}
proc pub:version {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    global ctcmd
        putserv "$ctcmd(output) :Name: $ctcmd(logo)"
        putserv "$ctcmd(output) :Version: $ctcmd(ver)"
        putserv "$ctcmd(output) :Author: $ctcmd(author)"
}
proc check:1 {chan} {
    global ctcmd
    if {![botisop $chan]} {
    putserv "$ctcmd(output) :I am Currently not Op'd on $chan."
    }
}
proc check:2 {chan} {
    global ctcmd
    if {![botisop $chan]} {
    putserv "$ctcmd(output) :I am Currently not Op'd on $chan."
    } elseif {$ctcmd(ircd) < "3"} { 
    return 
    }
}
set pingchan ""
proc pub:ping {nick host hand chan text} {
    global pingchan pingwho
    if {![check:auth $nick $hand]} {return 0}
    set pingwho [lindex [split $text] 0]
    if {$pingwho == ""} {set pingwho $nick}
    putquick "PRIVMSG $pingwho :\001PING [clock clicks -milliseconds]\001"
    set pingchan $chan
}
proc ct:pingr {nick uhost hand dest keyword args} {
    global pingchan ctcmd colo pingwho
    set time [expr {([clock clicks -milliseconds] - $args) / 1000.000}]
    putquick "$ctcmd(output) :\[\00303PING\003\] reply from $pingwho: \[\00303$time\003\] seconds "
}
proc pub:owner {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    pub:set:check $chan
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            putserv "MODE $chan +q $ctnick"
        }
        flushmode $chan
    } else {
        putquick "MODE $chan +q $nick"
    }
}
proc pub:deowner {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    pub:set:check $chan
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            putserv "MODE $chan -q $ctnick"
        }
        flushmode $chan
    } else {
        putquick "MODE $chan -q $nick"
    }
}
proc pub:protect {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    pub:set:check $chan
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            putserv "MODE $chan +a $ctnick"
        }
        flushmode $chan
    } else {
        putquick "MODE $chan +a $nick"
    }
}
proc pub:deprotect {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    pub:set:check $chan
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            putserv "MODE $chan -a $ctnick"
        }
        flushmode $chan
    } else {
        putquick "MODE $chan -a $nick"
    }
}
proc pub:op {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    if {![botisop $chan]} {return 0}
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            pushmode $chan +o $ctnick
        }
        flushmode $chan
    } else {
        putquick "MODE $chan +o $nick"
    }
}
proc pub:deop {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    if {![botisop $chan]} {return 0}
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            pushmode $chan -o $ctnick
        }
        flushmode $chan
    } else {
        putquick "MODE $chan -o $nick"
    }
}
proc pub:halfop {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    pub:set:check $chan
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            pushmode $chan +h $ctnick
        }
        flushmode $chan
    } else {
        putquick "MODE $chan +h $nick"
    }
}
proc pub:dehalfop {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    pub:set:check $chan
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            pushmode $chan -h $ctnick
        }
        flushmode $chan
    } else {
        putquick "MODE $chan -h $nick"
    }
}
proc pub:voice {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    if {![botisop $chan]} { return 0 }
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            pushmode $chan +v $ctnick
        }
        flushmode $chan
    } else {
        putquick "MODE $chan +v $nick"
    }
}
proc pub:devoice {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    if {![botisop $chan]} { return 0 }
    if {[lindex [split $text] 0 ] !=""} {
        foreach ctnick [split $text] {
            pushmode $chan -v $ctnick
        }
        flushmode $chan
    } else {
        putquick "MODE $chan -v $nick"
    }
}
proc pub:say {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    putserv "PRIVMSG $chan :$[lrange $text 0 end]"
}
proc pub:act {nick host hand chan text} {
    if {![check:auth $nick $hand ]} {return 0}
    putserv "PRIVMSG $chan :\001ACTION [lrange $text 0 end]\001"
}
proc pub:rehash {nick host hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    putserv "$ctcmd(output) :Rehashing"
    rehash
}
proc pub:restart {nick host hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    putserv "$ctcmd(output) :Restarting"
    restart
}
proc pub:die {nick host hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    putserv "$ctcmd(output) :Shutting Down"
    if {$text == ""} {
        die "Shutting Down..."
    } else {
        die $text
    }
}
proc pub:topic {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    if {![botisop $chan]} {return 0
    } else {
        putserv "TOPIC $chan :[lrange $text 0 end]"
    }
}
proc pub:chanset {nick host hand chan arg} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    foreach {set value} [split $arg] {break}
    if {![info exists value]} {
        catch {channel set $chan $set} error
    } {
        catch {channel set $chan $set $value} error
    }
    if {$error == ""} {
        puthelp "$ctcmd(output) :Successfully set $arg"
    } {
        puthelp "$ctcmd(output) :Error setting $arg: [lindex $error 0]..."
    }
}
proc pub:invite {nick host hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    putquick "INVITE [lindex [split $text] 0 ] $chan"
    putserv "$ctcmd(output) :Invited [lindex [split $text] 0 ] to $chan"
    putserv "$ctcmd(output) :You have been invited to $chan"
}
proc pub:mute {nick uhost hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    set var0 "[lindex [split $text] 0 ]"
    if {![botisop $chan]} {return 0}
    if {[isbotnick $var0]} {return 0}
    if {[matchattr [nick2hand $var0] n]} {return 0}
    if {[onchan $var0 $chan]} {
        set host "[maskhost $var0![getchanhost $var0 $chan] $ctcmd(btype)]"
switch -- $ctcmd(ircd) {
         1 {putquick "mode $chan +b ~q:$host"}
         2 {putquick "mode $chan +b m:$host"}
         3 {putquick "mode $chan +q $var0"}
         4 {putquick "mode $chan +b $var0"}
    }
  }
}
proc pub:unmute {nick host hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    set var0 "[lindex [split $text] 0 ]"
    if {![botisop $chan]} {return 0}
    if {[isbotnick $var0]} {return 0}
    if {[matchattr [nick2hand $var0] n]} {return 0}
    if {[onchan $var0 $chan]} {
        set host "[maskhost $var0![getchanhost $var0 $chan] $ctcmd(btype)]"
switch -- $ctcmd(ircd) {
        1 {putquick "mode $chan -b ~q:$host"}
        2 {putquick "mode $chan -b m:$host"}
        3 {putquick "mode $chan -q $var0"}
        4 {putquick "mode $chan -b $var0"}
    }
  }
}
proc pub:ban {nick host hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    global ctcmd
    set var0 [lindex [split $text] 0 ]
    if {![botisop $chan]} {
    } elseif {[isbotnick $kbnick] && [matchattr [nick2hand $kbnick] $ctcmd(flag)]} {
    } return }
    if {[onchan $var0 $chan]} {
        set bhost "[maskhost $var0![getchanhost $var0 $chan] $ctcmd(btype)]"
        putnow "mode $chan +b $bhost"
    } else {
        putnow "mode $chan +b $var0"
    }
}
proc pub:unban {nick uhost hand chan text} {
    if {![check:auth $nick $hand]} {return 0}
    global ctcmd
    set var0 [lindex [split $text] 0 ]
    if {![botisop $chan]} { return 0 }
    if {[onchan $var0 $chan]} {
        set host "[maskhost $var0![getchanhost $var0 $chan] $ctcmd(btype)]"
        putquick "mode $chan -b $host"
    } else {
        putnow "mode $chan -b $var0"
    }
}
proc pub:kickban {nick uhost hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    if {![botisop $chan]} { return 0 }
    set kbnick [lindex [split $text] 0 ]
    if {[isbotnick $kbnick] && [matchattr [nick2hand $kbnick] $ctcmd(flag)]} {
        return 
    } else {
        if {[lrange $text 1 end] !=""} {
            set reason "[lrange $text 1 end] ($nick)"
        } else {
            set reason "Requested kickban by $nick"
        }
        set host "[maskhost $kbnick![getchanhost $kbnick $chan] $ctcmd(btype)]"
        if {[onchan $kbnick $chan]} {
            pushmode $chan +b $host
            flushmode $chan
            putkick $chan $kbnick "$reason"
        }
    }
}
proc pub:kick {nick uhost hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    set 2kick [lindex [split $text] 0 ]
    if {![botisop $chan]} { return 0 }
    if {[isbotnick $2kick]} {
    return 0
    } elseif {[matchattr [nick2hand $2kick] $ctcmd(flag)]} { return 0
    } else {
    if {[lrange $text 2 end] !=""} {
    set reason "[lrange $text 1 end] ($nick)"
    } else {
    set reason "Requested kick by $nick"
    }
    if {[onchan $2kick $chan]} {
    putnow "kick $chan $2kick $reason"
        }
    }
}
proc pub:cycle {nick uhost hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    if {[lindex [split $text] 0 ] != ""} {
        set 2cycle [lindex [split $text] 0 ]
    } else {
        set 2cycle $chan
    }
    putserv "PART $2cycle"
    putserv "$ctcmd(out) Cycling $2cycle"
}
proc pub:join {nick uhost hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    if {[lindex [split $text] 0 ] != ""} {
        set 2join [lindex [split $text] 0 ]
    } else {
        set 2join $chan
    }
    channel add $2join
    putserv "$ctcmd(out) :Joining $2join"
}
proc pub:part {nick uhost hand chan text} {
global ctcmd
    if {![check:auth $nick $hand]} {return 0}
    if {[lindex [split $text] 0 ] != ""} {
        set 2part [lindex [split $text] 0 ]
    } else {
        set 2part $chan
    }
    channel remove $2part
    putserv "$ctcmd(out) :Parting $2part"
}
proc pub:mode {nick uhost hand chan text} {
if {![check:auth $nick $hand ]} {return 0}
    if {![botisop $chan]} {return 0
        set a2modes [join [lindex [split $text] 0]]
        if {[string first - $a2modes] >=0} {
            set chanmodes [getchanmode $chan]
            set modes [join [lindex [split $chanmodes] 0]]
            if {[string first k $modes]>0} {
                putserv "MODE $chan $text [join [lindex [split $chanmodes] 1]]"
            } else {
                putserv "MODE $chan $text"
         }
      }
   }
}
proc pub:adduser {nick host hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    set var0 "[lindex [split $text] 0 ]"
    if {[validuser $var0]} {return 1} ;putserv "NOTICE $nick :$var0 Is Already A Known User"
    set host0 "[maskhost $var0![getchanhost $var0 $chan] 2]"
    adduser $var0 $host0
    putserv "$ctcmd(output) :Added User: $var0 with Host: $host0"
    return
}
proc pub:deluser {nick host hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    set var0 "[lindex [split $text] 0 ]"
    if {![validuser $var0]} {return 0} ;putserv "NOTICE $nick :$var0 Isn't a Known User"
    deluser $var0
    putserv "$ctcmd(output) :Removed User: $var0"
    return
}
proc pub:chattr {nick host hand chan text} {
    global ctcmd
    set hand2catch [lindex [split $text] 0 ]
    set 2change [lindex [split $text] 1 ]
    set chan2edit [lindex [split $text] 2 ]
    if {[lindex [split $text] 3 ]} {
        chattr $hand2catch $2change $chan2edit
        putserv "$ctcmd(output) :Channel flags for $hand2catch on $chan2edit are now +$2change"
    } elseif {[lindex [split $text] 2 ]} {
        chattr $hand2catch $2change
        putserv "$ctcmd(output) :Global flags for $hand2catch are now +$2change"
    } elseif {[lindex [split $text] 0 ] !=""} {
        putserv "$ctcmd(output) :Usage: $ctcmd(trig)chattr <handle> \[changes\] \[channel\]"
    }
}
proc pub:addhost {nick host hand chan text} {
    global ctcmd
    if {[llength $text] == 0} {
        putserv "$ctcmd(output) :\[Syntax\]:  $ctcmd(trig))]addhost <nick/hand> \[host\]"
        return
    }
    set who7 [lindex [split $text] 0 ]
    set host5 [lindex [split $text] 1 ]
    if {[validuser $who7]} {
        setuser $who7 hosts $host5
        putserv "$ctcmd(output) :$host5 added for $who7"
        return
   } else {
        putserv "$ctcmd(output) :$who7 is not listed on the bot's userfile"
        return
    }
}
proc pub:delhost {nick host hand chan text} {
    global ctcmd
    if {[llength $text] == 0} {
    putserv "$ctcmd(output) :\[Syntax\]:  $ctcmd(trig)delhost <nick/hand> \[host\]"
    return
    }
    set who7 [lindex [split $text] 0 ]
    set host5 [lindex [split $text] 1 ]
    if {[validuser $who7]} {
    delhost $who7 $host5
    putserv "$ctcmd(output) :$host5 removed for $who7"
    } else {
    putserv "$ctcmd(output) :$who7 is not listed on the bot's userfile"
    return
    }
}
proc pub:addban {nick host hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    if {[llength [split $text]]} {
    putserv "$ctcmd(output) :Syntax: $ctcmd(trig)Addban <Nick/Host> <Reason> <Time>"
    return 
    }
    set bad [lindex [split $text] 0]
    set breason [lindex [split $text] 1]
    set btime [lindex [split $text] 2]
    set bd5 {[string index $btime end]}
    set bd6 {[string trimright $btime $bd5]}
	switch -- [string tolower $bd5] {
		d {set output [expr {$bd6 * 1440}]}
		h {set output [expr {$bd6 * 60}]}
		m {set output $bd6}
		default {putserv "$ctcmd(out) :Syntax. $ctcmd(trig)addban <amount/(d,m,h)>"; return 0}
	}
    set bhost "[maskhost $bad![getchanhost $bad $chan] $ctcmd(btype)]"
    if {[isban $bhost $chan]} {
    putserv "$ctcmd(out) :$bhost Is already a Ban on $chan Banlist"
    return
    }
    if {$breason == ""} { set breason "requested" }
    if {$btime == ""} { set btime $ctcmd(bant) }
    newchanban "$chan" "$bhost" "$nick" "$breason" $output
    
  }
proc pub:delban {nick uhost hand chan text} {
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    if {[llength [split $text]]} {
    putserv "$ctcmd(output) :Syntax: $ctcmd(trig)Delban <Nick/Host>"
    return
    }
    set unbanmask [lindex [split $text] 0]
    if {![isban $unbanmask]} {
    putserv "$ctcmd(output) :\037ERROR\037: Banmask not found."
    return
    killchanban $chan $unbanmask
    putserv "$ctcmd(output) :Successfully Deleted Ban: $unbanmask for $chan"
    }
  }
proc pub:bans {nick uhost hand chan text} {
global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    foreach bans [banlist $chan] {
    if {$bans == ""} {
    putserv "$ctcmd(output) :$chan Banlist is Empty"
    return 
    }
    set victim [lindex $bans 0]
    set why [lindex $bans 1]
    set expire [lindex $bans 2]
    set bexpire [ctime $expire]
    set who [lindex $bans 5]
    set remain {[expr $expire - [unixtime]]}
    putserv "$ctcmd(output) :$chan Banlist Start"
    putserv "$ctcmd(output) :\002Ban\002: $victim \002Expiration\002: $bexpire"
    putserv "$ctcmd(output) :\002Creator\002: $who: \002Reason\002: $why"
    putserv "$ctcmd(output) :$chan Banlist End"
    }
}
proc pub:addignore {nick host hand chan text} {
global ctcmd
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    if {[llength [split $text]]} { 
    putserv "$ctcmd(output) :Syntax: $ctcmd(trig)AddIgnore <Nick/Host> <Reason> <Time>" 
    return 0 
    }
    set bad [lindex [split $text] 0]
    set breason [lindex [split $text] 1]
    set btime [lindex [split $text] 2]
    set bhost "[maskhost $bad![getchanhost $bad $chan] 2]"
    newignore "$bhost" "$nick" "$breason" $btime
}
proc pub:delignore {nick uhost hand chan text} {
global ctcmd
    global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    if {[llength [split $text]]} { 
    putserv "$ctcmd(output) :Syntax: $ctcmd(trig)DelIgnore <Nick/Host>" 
    return 0
    }
    set unbanmask [lindex [split $text] 0]
    if {![isban $unbanmask ]} {
    putserv "$ctcmd(output) :\037ERROR\037: Banmask not found."; 
    return
    }
    killignore $unbanmask
    putquick "$ctcmd(output) :Successfully Deleted Ignore: $unbanmask"
  }
proc pub:ignores {nick uhost hand chan text} {
global ctcmd
    if {![check:auth $nick $hand ]} {return 0}
    if {[ignorelist] == ""} {
    putquick "$ctcmd(output) :\002There are Currently no Ignores\002"
  } else {
    putquick "$ctcmd(output) :Current Ignore List"
    foreach ignore [ignorelist] {
    set ctmask [lindex $ignore 0]
    set ctreason [lindex $ignore 1]
    set ctexpire [lindex $ignore 2]
    set ctadded [lindex $ignore 3]
    set ctcreator [lindex $ignore 4]
    set ctexpire:time [time $ctexpire]
    set ctadded:time [time $ctadded]
    if {$ctexpire == 0} {
    set banexpire:time "perm"
    }
    putserv "$ctcmd(output) :Ignore List"
    putserv "$ctcmd(output) :\002Hostmask\002: $ctmask - \002Creator\002: $ctcreator."
    putserv "$ctcmd(output) :\002Reason\002: $ctreason"
    putserv "$ctcmd(output) :\002Created\002: $ctadded:time. - \002Expiration\002: $ctexpire:time."
        }
    }
}
proc pub:whois {nick host hand chan arg} {
    global whois
    if {![check:auth $nick $hand ]} {return 0}
    set target [lindex [split $arg] 0]
    putquick "WHOIS $target $target"
    set ::nickout $nick
    set ::whoistarget $target
    set ::channel3 $chan
}
    bind RAW - 311 ctcommands::set:info
    bind RAW - 319 ctcommands::set:channels
    bind RAW - 301 ctcommands::set:away
    bind RAW - 313 ctcommands::set:ircop
    bind RAW - 330 ctcommands::set:auth
    bind RAW - 317 ctcommands::set:idle
proc set:putmsg {chan arg} {
global ctcmd
 set nick3 $::nickout
 set chan3 $::channel3 
 if {$ctcmd(msg) == "1"} { 
    putserv "NOTICE $nick3 :$arg"
 } else { 
    putserv "PRIVMSG $chan3 :$arg"
}
}
proc set:info {from keyword arg} {
    set chan $::channel3
    set nickname [lindex [split $arg] 1]
    set ident [lindex [split $arg] 2]
    set host [lindex [split $arg] 3]
    set realname [string range [join [lrange $arg 5 end]] 1 end]
    set:putmsg $chan "$nickname - $ident@$host * $realname"
}
proc set:ircop {from keyword arg} {
    set chan $::channel3
    set target $::whoistarget
    set:putmsg $chan "$target is an IRC Operator"
}
proc set:away {from keyword arg} {
    set chan $::channel3
    set target $::whoistarget
    set awaymessage [string range [join [lrange $arg 2 end]] 1 end]
    set:putmsg $chan "$target is away: $awaymessage"
}
proc set:channels {from keyword arg} {
    set chan $::channel3
    set channels [string range [join [lrange $arg 2 end]] 1 end]
    set target $::whoistarget
    set:putmsg $chan "$target on $channels"
}
proc set:auth {from keyword arg} {
    set chan $::channel3
    set target $::whoistarget
    set authname [lindex [split $arg] 2]
    set:putmsg $chan "$target is authed as $authname"
}
proc set:idle {from keyword arg} {
    set chan $::whoischannel
    set target $::whoistarget
    set idletime [lindex [split $arg] 2]
    set signon [lindex [split $arg] 3]
    set:putmsg $chan "$target has been idle for [duration $idletime]. signon time [ctime $signon]"
}
    putlog "\00302$ctcmd(logo) $ctcmd(ver) By $ctcmd(author) Loaded\002"
}
#################################################################################################################################################################

