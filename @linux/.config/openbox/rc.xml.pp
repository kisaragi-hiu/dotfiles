#lang pollen
<?xml version="1.0" encoding="UTF-8"?>

<!-- Openbox config -->

◊; Documentation index <"http://openbox.org/wiki/Help:Contents">
◊; Configuration documentation <"http://openbox.org/wiki/Help:Configuration">
◊; Actions documentation <"http://openbox.org/wiki/Help:Actions">
◊; Keybinding documentation <"http://openbox.org/wiki/Help:Bindings">

<openbox_config xmlns="http://openbox.org/3.4/rc"
                xmlns:xi="http://www.w3.org/2001/XInclude">

◊(xexpr->string*
  '(resistance
    (strength "10")
    (screen_edge_strength "20"))

  '(focus
    (focusNew "yes") ; new windows always get focus
    (followMouse "no") ; focus on hover
    ;; followMouse settings {{{
    (focusLast "yes") ; focus last used , not hovered, on desktop switch
    (underMouse "no") ; keep focus under cursor even when not moving
    (focusDelay "200") ; delay until focus change after hover
    ;; }}}
    (raiseOnFocus "no")) ; raise window if it gets focused

  '(placement
    (policy "Smart") ; "Smart" or "UnderMouse"
    (center "yes") ; place windows in center or top left of free area
    (monitor "Primary") ; place windows in: Any Mouse Active Primary
    (primaryMonitor "1")) ; primary monitor: 1-index Mouse Active

  `(theme
    (name "Arc-Dark")
    ;; a combination of NDSLIMC
    ;; N: icon, L: title; I: Iconify, M: Maximize
    ;; C: close, S: shade, D: omnipresent
    (titleLayout "NLIMC")
    (keepBorder "yes") ; when turning off window decoration
    (animateIconify "no")
    ,(font "ActiveWindow"            "Roboto" 10 "normal" "normal")
    ,(font "InactiveWindow"          "Roboto" 10 "normal" "normal")
    ,(font "MenuHeader"              "sans"   9  "normal" "normal")
    ,(font "MenuItem"                "sans"   9  "normal" "normal")
    ,(font "ActiveOnScreenDisplay"   "sans"   9  "bold"   "normal")
    ,(font "InactiveOnScreenDisplay" "sans"   9  "bold"   "normal"))

  '(desktops
    ;; Set at startup. Pagers can modify this on the fly.
    (number "6")
    (firstdesk "1")
    (names
    ; (name "Desktop 1")
    ; (name "Desktop 2")
    )
    (popupTime "2"))

  '(resize
    (drawContents "yes")
    (popupShow "Nonpixel") ; "Always" "Never" "Nonpixel" (like terminals)
    (popupPosition "Center") ; "Center" "Top" "Fixed"
    (popupFixedPosition (x "10") (y "10"))) ; "Fixed" coordinates

  ;; Reserve a portion on screen where windows will not cover when
  ;; maximized or when initially placed.
  '(margins
    (top "0")
    (bottom "0")
    (left "0")
    (right "0"))

  ;; Shown when one or more "dockapp"s are running.
  ;; https://wiki.archlinux.org/index.php/Window_Maker#Dockapps
  '(dock
    (position "TopLeft")
    (stacking "Above") ; which window layer? "Above" "Normal" "Below"
    (floatingX "0") (floatingY "0")
    ;; A "strut" stops windows from being placed or maximized over itself.
    (noStrut "no")
    (direction "Horizontal")
    (autoHide "no")
    (hideDelay "300")
    (showDelay "300")
    ;; the mouse button for moving individual dockapps
    (moveButton "Middle")))

<!-- keybindings -->
<keyboard>
  ◊(xexpr->string*
    '(chainQuitKey "C-g")
    `(keybind ([key "W-r"])
      (action ([name "Execute"]) (command "bash -c 'raco pollen render ~/.config/openbox/; openbox --reconfigure'"))
      ,(action/notify "Reconfiguring Openbox" #:icon "view-refresh" #:return-xexpr? #t)))

  <!-- Keybindings for desktop switching and window manipulation -->
  ◊(xexpr->string*
    `(keybind ([key "W-w"] [chroot "true"])
      (keybind ([key "Escape W-w"]) (action ([name "BreakChroot"])))
      (keybind ([key "t"] [chroot "true"])
       (keybind ([key "Escape t"]) (action ([name "BreakChroot"])))
       (keybind ([key "Down j"]) ,(action/execute "compton-trans" "-c" "-10" #:return-xexpr? #t))
       (keybind ([key "Up k"]) ,(action/execute "compton-trans" "-c" "+10" #:return-xexpr? #t)))

      (keybind ([key "m"] [chroot "true"])
       (keybind ([key "Escape m"]) (action ([name "BreakChroot"])))
       (keybind ([key "c"])
        (action ([name "MoveResizeTo"])
         (x "center") (y "center")))
       (keybind ([key "Left h"])
        (action ([name "MoveRelative"])
         (x "-10") (y "0")))
       (keybind ([key "Right l"])
        (action ([name "MoveRelative"])
         (x "10") (y "0")))
       (keybind ([key "Down j"])
        (action ([name "MoveRelative"])
         (x "0") (y "10")))
       (keybind ([key "Up k"])
        (action ([name "MoveRelative"])
         (x "0") (y "-10"))))

      (keybind ([key "r"] [chroot "true"]) ; resize (grow)
       (keybind ([key "Escape r"]) (action ([name "BreakChroot"])))
       (keybind ([key "Left h"])
        (action ([name "ResizeRelative"])
         (left "10")))
       (keybind ([key "Right l"])
        (action ([name "ResizeRelative"])
         (right "10")))
       (keybind ([key "Down j"])
        (action ([name "ResizeRelative"])
         (down "10")))
       (keybind ([key "Up k"])
        (action ([name "ResizeRelative"])
         (up "10"))))

     ; l/r ↑/↓ swapped to match movement direction
      (keybind ([key "S-r"] [chroot "true"]) ; resize (shrink)
       (keybind ([key "Escape S-r"]) (action ([name "BreakChroot"])))
       (keybind ([key "Left h"])
        (action ([name "ResizeRelative"])
         (right "-10")))
       (keybind ([key "Right l"])
        (action ([name "ResizeRelative"])
         (left "-10")))
       (keybind ([key "Down j"])
        (action ([name "ResizeRelative"])
         (up "-10")))
       (keybind ([key "Up k"])
        (action ([name "ResizeRelative"])
         (down "-10"))))

      (keybind ([key "Left h"])
       (action ([name "GoToDesktop"])
        (to "left") (wrap "yes")))
      (keybind ([key "Right l"])
       (action ([name "GoToDesktop"])
        (to "right") (wrap "yes")))
      (keybind ([key "Down j"])
       (action ([name "GoToDesktop"])
        (to "down") (wrap "yes")))
      (keybind ([key "Up k"])
       (action ([name "GoToDesktop"])
        (to "up") (wrap "yes")))))

  <keybind key="C-W-Left C-W-h">
    <action name="GoToDesktop"><to>left</to><wrap>yes</wrap></action>
  </keybind>
  <keybind key="C-W-Right C-W-l">
    <action name="GoToDesktop"><to>right</to><wrap>yes</wrap></action>
  </keybind>
  <keybind key="C-W-Up C-W-k">
    <action name="GoToDesktop"><to>up</to><wrap>yes</wrap></action>
  </keybind>
  <keybind key="C-W-Down C-W-j">
    <action name="GoToDesktop"><to>down</to><wrap>yes</wrap></action>
  </keybind>

  <!-- edge snap -->
  <keybind key="W-S-Left W-S-h">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="Maximize"><direction>vertical</direction></action>
    <action name="MoveResizeTo"><width>50%</width></action>
    <action name="MoveToEdge"><direction>west</direction></action>
  </keybind>
  <keybind key="W-S-Right W-S-l">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="Maximize"><direction>vertical</direction></action>
    <action name="MoveResizeTo"><width>50%</width></action>
    <action name="MoveToEdge"><direction>east</direction></action>
  </keybind>
  <keybind key="W-S-Up W-S-k">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="Maximize"><direction>horizontal</direction></action>
    <action name="MoveResizeTo"><height>50%</height></action>
    <action name="MoveToEdge"><direction>north</direction></action>
  </keybind>
  <keybind key="W-S-Down W-S-j">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="Maximize"><direction>horizontal</direction></action>
    <action name="MoveResizeTo"><height>50%</height></action>
    <action name="MoveToEdge"><direction>south</direction></action>
  </keybind>

  <!-- grid snap -->
  <!-- shortcuts subject to change -->
  <keybind key="C-W-S-Left C-W-S-h">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="MoveResizeTo"><width>50%</width><height>50%</height></action>
    <action name="MoveToEdge"><direction>west</direction></action>
    <action name="MoveToEdge"><direction>north</direction></action>
  </keybind>
  <keybind key="C-W-S-Right C-W-S-l">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="MoveResizeTo"><width>50%</width><height>50%</height></action>
    <action name="MoveToEdge"><direction>west</direction></action>
    <action name="MoveToEdge"><direction>south</direction></action>
  </keybind>
  <keybind key="C-W-S-Up C-W-S-k">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="MoveResizeTo"><width>50%</width><height>50%</height></action>
    <action name="MoveToEdge"><direction>east</direction></action>
    <action name="MoveToEdge"><direction>south</direction></action>
  </keybind>
  <keybind key="C-W-S-Down C-W-S-j">
    <action name="Unmaximize"><direction>both</direction></action>
    <action name="MoveResizeTo"><width>50%</width><height>50%</height></action>
    <action name="MoveToEdge"><direction>east</direction></action>
    <action name="MoveToEdge"><direction>north</direction></action>
  </keybind>

  <keybind key="W-A-Left W-A-h">
    <action name="SendToDesktop"><to>left</to><wrap>no</wrap></action>
  </keybind>
  <keybind key="W-A-Right W-A-l">
    <action name="SendToDesktop"><to>right</to><wrap>no</wrap></action>
  </keybind>
  <keybind key="W-A-Up W-A-k">
    <action name="SendToDesktop"><to>up</to><wrap>no</wrap></action>
  </keybind>
  <keybind key="W-A-Down W-A-j">
    <action name="SendToDesktop"><to>down</to><wrap>no</wrap></action>
  </keybind>

  <keybind key="W-d">
    <action name="ToggleShowDesktop"/>
  </keybind>

  <!-- keybinds that act on windows -->
  <keybind key="A-F4">
    <action name="Close"/>
  </keybind>
  ◊(xexpr->string*
    '(keybind ([key "W-F9 A-Menu"])
      (action ([name "ShowMenu"])
       (menu "client-menu")))
    '(keybind ([key "W-F10"])
      (action ([name "ToggleDecorations"])))
    '(keybind ([key "W-F11"])
      (action ([name "ToggleFullscreen"])))
    '(keybind ([key "W-Up W-k"])
      (action ([name "ToggleMaximize"])))
    '(keybind ([key "W-Down W-j"])
      (action ([name "Iconify"]))))

  <!-- Alt-tabbing -->
  <keybind key="A-Tab Menu">
    <action name="NextWindow">
      <finalactions>
        <action name="Focus"/>
        <action name="Raise"/>
        <action name="Unshade"/>
      </finalactions>
    </action>
  </keybind>
  <keybind key="A-S-Tab S-Menu">
    <action name="PreviousWindow">
      <finalactions>
        <action name="Focus"/>
        <action name="Raise"/>
        <action name="Unshade"/>
      </finalactions>
    </action>
  </keybind>
  <keybind key="W-Tab">
    <action name="Execute">
      <command>rofi -show window</command>
    </action>
  </keybind>

  <!-- one-off command stuff -->
  <!-- Restart PulseAudio with W-p -->
  <keybind key="W-p">
    <action name="Execute"><command>pulseaudio -k</command></action>
    <action name="Execute"><command>pulseaudio --start</command></action>
    <action name="Execute"><command>notify-send "PulseAudio has been restarted" " " --icon view-refresh</command></action>
  </keybind>
  <keybind key="Pause">
    <action name="Execute"><command>bash -c "touch /tmp/trigger; sleep 1.2; rm /tmp/trigger"</command></action>
  </keybind>

  <!-- "extended" commands, or interactive actions -->
  <keybind key="W-x">
    <keybind key="Escape W-x"><action name="BreakChroot"/></keybind>
    <keybind key="c">
      ◊action/execute/konsole{bash -c 'echo "Write some stuff to copy." | vipe | copy'}
    </keybind>
    <keybind key="e">
      ◊action/execute/konsole{bash -c 'echo "echo \"Write a bash command here and copy its output\"" | vipe | bash | copy'}
    </keybind>
    <keybind key="f">
      ◊action/execute/konsole{bash -c 'cd $(echo "/path/to/search/with/fzf" | vipe) && fzf | copy'}
    </keybind>
  </keybind>

  <!-- Switch fcitx input methods with [S-]F35 -->
  <keybind key="F35">
    <action name="Execute">
      <command>fcitx-next-im</command>
    </action>
  </keybind>
  <keybind key="S-F35">
    <action name="Execute">
      <command>fcitx-next-im prev</command>
    </action>
  </keybind>

  <!-- W-Print to start Spectacle, or just Print to use imagemagick -->
  <keybind key="Print">
    <action name="Execute">
      <command>fish -c 'screenshot "$XDG_PICTURES_DIR"/Screenshots/Screenshot_(date +%Y%m%d_%H%M%S).jpg; notify-send "Screenshot taken" " " --icon accessories-screenshot'</command>
    </action>
  </keybind>
  <keybind key="W-Print">
    <action name="Execute">
      <command>spectacle</command>
    </action>
  </keybind>

  <!-- W-m for media controls -->
  <!-- Multibind (like "j Down") seems to break "chrooting" -->
  <keybind key="W-m" chroot="true">
    <keybind key="Escape W-m"><action name="BreakChroot"/></keybind>
    <keybind key="l">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/mediacontrol invokeShortcut nextmedia</command></action>
    </keybind>
    <keybind key="h">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/mediacontrol invokeShortcut previousmedia</command></action>
    </keybind>
    <keybind key="p"> <!-- p for pause. bound to toggle pause/play in plasma. -->
      <action name="Execute"><command>notify-send "Media Controls" "Pause toggled" --icon player_pause</command></action>
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/mediacontrol invokeShortcut playpausemedia</command></action>
    </keybind>
    <keybind key="s"> <!-- s for stop -->
      <action name="Execute"><command>notify-send "Media Controls" "Audio stopped" --icon player_stop</command></action>
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/mediacontrol invokeShortcut stopmedia</command></action>
    </keybind>
    <keybind key="m"> <!-- Toggle mute with W-m m -->
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut mute</command></action>
    </keybind>
    <keybind key="j">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut decrease_volume</command></action>
    </keybind>
    <keybind key="k">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut increase_volume</command></action>
    </keybind>
    <keybind key="S-j">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut decrease_microphone_volume</command></action>
    </keybind>
    <keybind key="S-k">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut increase_microphone_volume</command></action>
    </keybind>
  </keybind>

  <!-- Keybindings for running applications -->
  ◊(xexpr->string*
    (keybind "Scroll_Lock" (action/execute #:return-xexpr? #t
                                           "qdbus" "org.kde.kglobalaccel" "/component/yakuake" "invokeShortcut" "toggle-window-state"))
    (keybind "A-F1" (action/execute #:return-xexpr? #t
                                    "rofi" "-combi-modi" "drun,run,window" "-show" "combi" "-modi" "combi"))
    (keybind "W-b" (action/execute #:return-xexpr? #t
                                   "rofi-toggle" "plank" "xcompmgr"))
    (keybind "W-l" (action/execute #:return-xexpr? #t
                                   "oblogout"))
    ; (keybind "W-b" (action/execute #:return-xexpr? #t
    ;                                "toggle" "plank"))
    (keybind "W-s" (action/execute #:return-xexpr? #t
                                   "rofi-surfraw")))

  <!-- keymap -->
  <keybind key="W-o">
    <action name="Execute"><command>bash -c 'xmodmap &lt;(cat ~/.Xmodmap ~/.Xmodmap.d/osu.xmodmap)'</command></action>
    <action name="Execute"><command>wacom-sensitivity -c -s 578</command></action>
    <action name="Execute"><command>notify-send "Using osu! keymap" " "</command></action>
  </keybind>
  <keybind key="W-u">
    <action name="Execute"><command>bash -c 'xmodmap &lt;(cat ~/.Xmodmap ~/.Xmodmap.d/utau.xmodmap)'</command></action>
    <action name="Execute"><command>notify-send "Using utau keymap" " "</command></action>
  </keybind>
  <keybind key="W-S-u">
    <action name="Execute"><command>bash -c 'xmodmap ~/.Xmodmap.d/utau-reset.xmodmap'</command></action>
    <action name="Execute"><command>notify-send "Reset utau keymap" " "</command></action>
  </keybind>
  <keybind key="W-S-o">
    <action name="Execute"><command>bash -c 'xmodmap ~/.Xmodmap.d/osu-reset.xmodmap'</command></action>
    <action name="Execute"><command>wacom-sensitivity -c -s 950</command></action>
    <action name="Execute"><command>notify-send "Reset osu keymap" " "</command></action>
  </keybind>
  <!-- keybinds for osu!. use a keymap to bind stuff to F32-34 -->
  <keybind key="F30"> <!-- KP_6 in osu keymap -->
    <action name="Execute"><command>xdotool key shift+Tab</command></action>
  </keybind>
  <keybind key="F31">
    <action name="Execute"><command>xdotool key F2</command></action>
  </keybind>
  <keybind key="F32">
    <action name="Execute"><command>xdotool click 2</command></action>
  </keybind>
  <keybind key="F33">
    <action name="Execute"><command>xdotool key Tab</command></action>
  </keybind>
  <keybind key="F34">
    <action name="Execute"><command>bash -c "xdotool keydown grave; sleep 0.7; xdotool keyup grave"</command></action>
  </keybind>
  <!-- utau keybinds -->
  <keybind key="F19">
    ◊(action/execute "xdotool" "key" "space")
  </keybind>
  <keybind key="F20">
    ◊(action/execute "xdotool" "key" "control+a")
  </keybind>
  <keybind key="F21">
    ◊(action/execute "xdotool" "key" "control+e")
  </keybind>
  <keybind key="F22">
    ◊(action/execute "xdotool" "key" "control+t")
  </keybind>
  <keybind key="F23">
    ◊(action/execute "xdotool" "key" "control+y")
  </keybind>
  <keybind key="F24">
    ◊(action/execute "xdotool" "key" "control+z")
  </keybind>
  <keybind key="F25">
    ◊(action/execute "xdotool" "key" "control+s")
  </keybind>
  <keybind key="F26">
    ◊(action/execute "xdotool" "key" "control+Shift+z")
  </keybind>
  <keybind key="F27">
    ◊(action/execute "xdotool" "key" "b")
  </keybind>
  <keybind key="F29">
    ◊(action/execute "xdotool" "key" "n")
  </keybind>
  <!-- application specific keybinds -->
  <keybind key="W-a" chroot="true">
    <keybind key="Escape W-a"><action name="BreakChroot"/></keybind>
    <keybind key="o" chroot="true"> <!-- osu -->
      <keybind key="Escape o"><action name="BreakChroot"/></keybind>
      <keybind key="s"> <!-- sorting -->
        <keybind key="a">
          <action name="Execute"><command>◊osu-select-sorting["artist"]</command></action>
        </keybind>
        <keybind key="b">
          <action name="Execute"><command>◊osu-select-sorting["bpm"]</command></action>
        </keybind>
        <keybind key="c">
          <action name="Execute"><command>◊osu-select-sorting["creator"]</command></action>
        </keybind>
        <keybind key="d">
          <action name="Execute"><command>◊osu-select-sorting["date"]</command></action>
        </keybind>
        <keybind key="e">
          <action name="Execute"><command>◊osu-select-sorting["difficulty"]</command></action>
        </keybind>
        <keybind key="l">
          <action name="Execute"><command>◊osu-select-sorting["length"]</command></action>
        </keybind>
        <keybind key="r">
          <action name="Execute"><command>◊osu-select-sorting["rank"]</command></action>
        </keybind>
        <keybind key="t">
          <action name="Execute"><command>◊osu-select-sorting["title"]</command></action>
        </keybind>
      </keybind>
      <keybind key="KP_6"> <!-- KP_6 in osu keymap -->
        <action name="Execute"><command>xdotool key shift+Tab</command></action>
      </keybind>
      <keybind key="KP_8">
        <action name="Execute"><command>xdotool key F2</command></action>
      </keybind>
      <keybind key="KP_7">
        <action name="Execute"><command>xdotool click 2</command></action>
      </keybind>
      <keybind key="KP_4">
        <action name="Execute"><command>xdotool key Tab</command></action>
      </keybind>
      <keybind key="KP_5">
        <action name="Execute"><command>bash -c "xdotool keydown grave; sleep 0.7; xdotool keyup grave"</command></action>
      </keybind>
    </keybind>
    <keybind key="u" chroot="true">
      <keybind key="Escape u"><action name="BreakChroot"/></keybind>
      <keybind key="KP_Decimal">
        ◊(action/execute "xdotool" "key" "space")
      </keybind>
      <keybind key="KP_0">
        ◊(action/execute "xdotool" "key" "control+a")
      </keybind>
      <keybind key="KP_1">
        ◊(action/execute "xdotool" "key" "control+e")
      </keybind>
      <keybind key="KP_2">
        ◊(action/execute "xdotool" "key" "control+t")
      </keybind>
      <keybind key="KP_3">
        ◊(action/execute "xdotool" "key" "control+y")
      </keybind>
      <keybind key="KP_4">
        ◊(action/execute "xdotool" "key" "control+z")
      </keybind>
      <keybind key="KP_5">
        ◊(action/execute "xdotool" "key" "control+s")
      </keybind>
      <keybind key="KP_6">
        ◊(action/execute "xdotool" "key" "control+Shift+z")
      </keybind>
      <keybind key="KP_7">
        ◊(action/execute "xdotool" "key" "b")
      </keybind>
      <keybind key="KP_9">
        ◊(action/execute "xdotool" "key" "n")
      </keybind>
    </keybind>
  </keybind>
</keyboard>

<mouse>
  <dragThreshold>1</dragThreshold>
  <!-- number of pixels the mouse must move before a drag begins -->
  <doubleClickTime>500</doubleClickTime>
  <!-- in milliseconds (1000 = 1 second) -->
  <screenEdgeWarpTime>400</screenEdgeWarpTime>
  <!-- Time before changing desktops when the pointer touches the edge of the
       screen while moving a window, in milliseconds (1000 = 1 second).
       Set this to 0 to disable warping -->
  <screenEdgeWarpMouse>false</screenEdgeWarpMouse>
  <!-- Set this to TRUE to move the mouse pointer across the desktop when
       switching due to hitting the edge of the screen -->

  ◊(context "Frame"
    (mousebind "W-Left" #:action "Press"
     (action "Focus")
     (action "Raise"))
    (mousebind "W-Left" #:action "Click"
     (action "Unshade"))
    (mousebind "W-Left" #:action "Drag"
     (action "Unmaximize")
     (action "Move"))

    ; (mousebind "W-Right" #:action "Press"
    ;  (action "Focus")
    ;  (action "Raise")
    ;  (action "Unshade"))
    (mousebind "W-Right" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action/menu "client-menu"))
    (mousebind "W-Right" #:action "Drag"
     (action "Resize"))

    (mousebind "W-Middle" #:action "Press"
     (action "Lower")
     (action "FocusToBottom")
     (action "Unfocus"))
    (mousebind "W-Up" #:action "Click"
     (action/goto-desktop "previous"))
    (mousebind "W-Down" #:action "Click"
     (action/goto-desktop "next"))
    (mousebind "W-A-Up" #:action "Click"
     (action/sendto-desktop "previous"))
    (mousebind "W-A-Down" #:action "Click"
     (action/sendto-desktop "next")))

  <context name="Titlebar">
    <mousebind button="Left" action="Drag">
      <action name="Unmaximize"/>
      <action name="Move"/>
    </mousebind>
    <mousebind button="Left" action="DoubleClick">
      <action name="ToggleMaximize"/>
    </mousebind>
  </context>

  <context name="Titlebar Top Right Bottom Left TLCorner TRCorner BRCorner BLCorner">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>

    <!-- middle click to minimize -->
    <mousebind button="Middle" action="Press">
      <action name="Iconify"/>
    </mousebind>

    <mousebind button="Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="ShowMenu"><menu>client-menu</menu></action>
    </mousebind>
  </context>

  <context name="Top">
    <mousebind button="Left" action="Drag">
      <action name="Resize"><edge>top</edge></action>
    </mousebind>
  </context>

  <context name="Left">
    <mousebind button="Left" action="Drag">
      <action name="Resize"><edge>left</edge></action>
    </mousebind>
  </context>

  ◊(context "Right"
    (mousebind "Left" #:action "Drag"
     (action "Resize" '(edge "right"))))

  ◊(context "Bottom"
    (mousebind "Left" #:action "Drag"
     (action "Resize" '(edge "bottom")))
    (mousebind "Right" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action/menu "client-menu")))

  ◊(context "TRCorner BRCorner TLCorner BLCorner"
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action "Unshade"))
    (mousebind "Left" #:action "Drag"
     (action "Resize")))

  ◊(context "Client"
    (mousebind "W-Up" #:action "Click"
     (action/goto-desktop "previous"))
    (mousebind "W-Down" #:action "Click"
     (action/goto-desktop "next"))
    (mousebind "Left Middle Right" #:action "Press"
     (action "Focus")
     (action "Raise")))

  ◊(context "Icon"
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action "Unshade")
     (action/menu "client-menu"))
    (mousebind "Right" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action/menu "client-menu")))

  ◊(context "AllDesktops"
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action "Unshade"))
    (mousebind "Left" #:action "Click"
     (action "ToggleOmnipresent")))

  ◊(context "Shade"
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise"))
    (mousebind "Left" #:action "Click"
     (action "ToggleShade")))

  ◊(context "Iconify"
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise"))
    (mousebind "Left" #:action "Click"
     (action "Iconify")))

  ◊(context "Maximize"
    (mousebind "Left Middle Right" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action "Unshade"))
    (mousebind "Left" #:action "Click"
     (action "ToggleMaximize"))
    (mousebind "Middle" #:action "Click"
     (action "ToggleMaximize" '(direction "vertical")))
    (mousebind "Right" #:action "Click"
     (action "ToggleMaximize" '(direction "horizontal"))))

  ◊(context "Close"
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise")
     (action "Unshade"))
    (mousebind "Left" #:action "Click"
     (action "Close")))

  ◊(context "Desktop"
    (mousebind "Up W-Up" #:action "Click"
     (action/goto-desktop "previous"))
    (mousebind "Down W-Down" #:action "Click"
     (action/goto-desktop "next"))
    (mousebind "Left" #:action "Press"
     (action "Focus")
     (action "Raise"))
    (mousebind "Right" #:action "Press"
     (action "Focus")
     (action "Raise")))

  ◊(context "Root"
    (mousebind "Middle" #:action "Press"
     (action/menu "client-list-combined-menu"))
    (mousebind "Right" #:action "Press"
     (action/menu "root-menu")))

  ◊(context "MoveResize"
    (mousebind "Up A-Up" #:action "Click"
     (action/goto-desktop "previous"))
    (mousebind "Down A-Down" #:action "Click"
     (action/goto-desktop "next")))
</mouse>

◊(xexpr->string*
  '(menu
    ;; more than one menu files can be specified
    (file "menu.xml")
    ;; if a press-release lasts longer than this, the menu hides again
    (hideDelay "200")
    ;; center submenus vertically about the parent entry
    (middle "no")
    ;; Time between hovering an entry and its submenu opening. Negative means don't show on hover.
    (submenuShowDelay "100")
    ;; time to delay before hiding a submenu when selecting another entry in parent menu
    ;; negative: submenu will not be hidden until a different submenu is opened
    (submenuHideDelay "400")
    (showIcons "yes")
    (manageDesktops "yes")))

◊(xexpr->string*
  `(applications
    (application ([class "*"])
     (decor "no"))

    ,@(application-match-multiple
       '(([class "Ardour"])
         ([class "isoimagewriter"]))
       '(decor "yes"))

   ; position plasma osd
    (application ([name "plasmashell"]
                  [class "plasmashell"]
                  [type "normal"])
     (position ([force "no"])
      (x "center") (y "60%"))
     (desktop "all"))
    (application ([name "wineconsole.exe"]
                  [type "normal"])
     (iconic "yes")
     (layer "below"))
    (application ([name "megasync"]
                  [title "MEGAsync"]
                  [type "normal"])
     (iconic "yes"))
   ; plank, please stop showing latte-dock…
    (application ([name "latte-dock"]
                  [class "lattedock"]
                  [type "dock"])
     (skip_pager "yes")
     (skip_taskbar "yes"))))
</openbox_config>

◊; Local Variables:
◊; mode: pollen
◊; End:
◊; vim: filetype=pollen
