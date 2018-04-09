#lang pollen
<?xml version="1.0" encoding="UTF-8"?>

<!-- Openbox config -->

<!-- Documentation index <"http://openbox.org/wiki/Help:Contents"> -->
<!-- Configuration documentation <"http://openbox.org/wiki/Help:Configuration"> -->
<!-- Actions documentation <"http://openbox.org/wiki/Help:Actions"> -->
<!-- Keybinding documentation <"http://openbox.org/wiki/Help:Bindings"> -->

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
    (name "Adapta-Nokto")
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
  <chainQuitKey>C-g</chainQuitKey>
  ◊; this is asyncronous. might need to W-r twice to use the newly rendered config
  <keybind key="W-r">
    <action name="Execute"><command>bash -c 'raco pollen render ~/.config/openbox/;openbox --reconfigure'</command></action>
    ◊action/notify["Openbox --reconfigure complete" #:icon "view-refresh"]
  </keybind>

  <!-- Keybindings for desktop switching and window manipulation -->
  <keybind key="W-w" chroot="true">
    <keybind key="Escape W-w"><action name="BreakChroot"/></keybind>
    <keybind key="t" chroot="true"> ◊; transparency
      <keybind key="Escape t"><action name="BreakChroot"/></keybind>
      <keybind key="Down j">
        ◊(action/execute "compton-trans" "-c" "-10")
      </keybind>
      <keybind key="Up k">
        ◊(action/execute "compton-trans" "-c" "+10")
      </keybind>
    </keybind>
    ◊(xexpr->string*
      '(keybind ([key "m"] [chroot "true"])
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

      '(keybind ([key "r"] [chroot "true"]) ; resize (grow)
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
      '(keybind ([key "S-r"] [chroot "true"]) ; resize (shrink)
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
          (down "-10")))))
    <keybind key="Left h">
      <action name="GoToDesktop"><to>left</to><wrap>yes</wrap></action>
    </keybind>
    <keybind key="Right l">
      <action name="GoToDesktop"><to>right</to><wrap>yes</wrap></action>
    </keybind>
    <keybind key="Up k">
      <action name="GoToDesktop"><to>up</to><wrap>yes</wrap></action>
    </keybind>
    <keybind key="Down j">
      <action name="GoToDesktop"><to>down</to><wrap>yes</wrap></action>
    </keybind>
  </keybind>

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
    '(keybind ([key "W-F9"])
      (action ([name "ShowMenu"])
       (menu "client-menu")))
    '(keybind ([key "W-F10"])
      (action ([name "ToggleDecorations"])))
    '(keybind ([key "W-F11"])
      (action ([name "ToggleFullscreen"]))))
  <keybind key="W-Up W-k">
    <!-- Going with no decorations by default now -->
    <action name="ToggleMaximize"/>
  </keybind>
  <keybind key="W-Down W-j">
    <action name="Iconify"/>
  </keybind>

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
      <command>bash -c 'import -window root "$XDG_PICTURES_DIR"/Screenshots/Screenshot_$(date +%Y%m%d_%H%M%S).jpg'</command>
    </action>
    <action name="Execute">
      <command>notify-send "Screenshot taken" " " --icon accessories-screenshot</command>
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
  ◊(keybind "Scroll_Lock" (action/execute #:return-xexpr? #t
                                          "qdbus" "org.kde.kglobalaccel" "/component/yakuake" "invokeShortcut" "toggle-window-state"))

  ◊(keybind "A-F1" (action/execute #:return-xexpr? #t
                                   "rofi" "-combi-modi" "drun,run,window" "-show" "combi" "-modi" "combi"))
  ◊(keybind "W-s" (action/execute #:return-xexpr? #t
                                  "rofi-surfraw"))

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

  <context name="Frame">
    ◊(xexpr->string*
      '(mousebind
        ([button "W-Left"] [action "Press"])
        (action ([name "Focus"]))
        (action ([name "Raise"])))
      '(mousebind
        ([button "W-Left"] [action "Click"])
        (action ([name "Unshade"])))
      '(mousebind
        ([button "W-Left"] [action "Drag"])
        (action ([name "Unmaximize"]))
        (action ([name "Move"]))))

    <mousebind button="W-Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="W-Right" action="Drag">
      <action name="Resize"/>
    </mousebind>

    <mousebind button="W-Middle" action="Press">
      <action name="Lower"/>
      <action name="FocusToBottom"/>
      <action name="Unfocus"/>
    </mousebind>

    <!-- Up Down = mwheelup/down -->
    <mousebind button="W-Up" action="Click">
      <action name="GoToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="W-Down" action="Click">
      <action name="GoToDesktop"><to>next</to></action>
    </mousebind>
    <mousebind button="W-S-Up" action="Click">
      <action name="SendToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="W-S-Down" action="Click">
      <action name="SendToDesktop"><to>next</to></action>
    </mousebind>
  </context>

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

  <context name="Right">
    <mousebind button="Left" action="Drag">
      <action name="Resize"><edge>right</edge></action>
    </mousebind>
  </context>

  <context name="Bottom">
    <mousebind button="Left" action="Drag">
      <action name="Resize"><edge>bottom</edge></action>
    </mousebind>

    <mousebind button="Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="ShowMenu"><menu>client-menu</menu></action>
    </mousebind>
  </context>

  <context name="TRCorner BRCorner TLCorner BLCorner">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="Left" action="Drag">
      <action name="Resize"/>
    </mousebind>
  </context>

  <context name="Client">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
    <mousebind button="Middle" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
    <mousebind button="Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
  </context>

  <context name="Icon">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
      <action name="ShowMenu"><menu>client-menu</menu></action>
    </mousebind>
    <mousebind button="Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="ShowMenu"><menu>client-menu</menu></action>
    </mousebind>
  </context>

  <context name="AllDesktops">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="Left" action="Click">
      <action name="ToggleOmnipresent"/>
    </mousebind>
  </context>

  <context name="Shade">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
    <mousebind button="Left" action="Click">
      <action name="ToggleShade"/>
    </mousebind>
  </context>

  <context name="Iconify">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
    <mousebind button="Left" action="Click">
      <action name="Iconify"/>
    </mousebind>
  </context>

  <context name="Maximize">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="Middle" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="Left" action="Click">
      <action name="ToggleMaximize"/>
    </mousebind>
    <mousebind button="Middle" action="Click">
      <action name="ToggleMaximize"><direction>vertical</direction></action>
    </mousebind>
    <mousebind button="Right" action="Click">
      <action name="ToggleMaximize"><direction>horizontal</direction></action>
    </mousebind>
  </context>

  <context name="Close">
    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="Left" action="Click">
      <action name="Close"/>
    </mousebind>
  </context>

  <context name="Desktop">
    <mousebind button="Up" action="Click">
      <action name="GoToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="Down" action="Click">
      <action name="GoToDesktop"><to>next</to></action>
    </mousebind>

    <mousebind button="A-Up" action="Click">
      <action name="GoToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="A-Down" action="Click">
      <action name="GoToDesktop"><to>next</to></action>
    </mousebind>
    <mousebind button="C-A-Up" action="Click">
      <action name="GoToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="C-A-Down" action="Click">
      <action name="GoToDesktop"><to>next</to></action>
    </mousebind>

    <mousebind button="Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
    <mousebind button="Right" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
  </context>

  <context name="Root">
    <!-- Menus -->
    <mousebind button="Middle" action="Press">
      <action name="ShowMenu"><menu>client-list-combined-menu</menu></action>
    </mousebind>
    <mousebind button="Right" action="Press">
      <action name="ShowMenu"><menu>root-menu</menu></action>
    </mousebind>
  </context>

  <context name="MoveResize">
    <mousebind button="Up" action="Click">
      <action name="GoToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="Down" action="Click">
      <action name="GoToDesktop"><to>next</to></action>
    </mousebind>
    <mousebind button="A-Up" action="Click">
      <action name="GoToDesktop"><to>previous</to></action>
    </mousebind>
    <mousebind button="A-Down" action="Click">
      <action name="GoToDesktop"><to>next</to></action>
    </mousebind>
  </context>
</mouse>

<menu>
  <!-- You can specify more than one menu file in here and they are all loaded,
       just don't make menu ids clash or, well, it'll be kind of pointless -->

  <!-- default menu file (or custom one in $HOME/.config/openbox/) -->
  <file>menu.xml</file>
  <hideDelay>200</hideDelay>
  <!-- if a press-release lasts longer than this setting (in milliseconds), the
       menu is hidden again -->
  <middle>no</middle>
  <!-- center submenus vertically about the parent entry -->
  <submenuShowDelay>100</submenuShowDelay>
  <!-- time to delay before showing a submenu after hovering over the parent
       entry.
       if this is a negative value, then the delay is infinite and the
       submenu will not be shown until it is clicked on -->
  <submenuHideDelay>400</submenuHideDelay>
  <!-- time to delay before hiding a submenu when selecting another
       entry in parent menu
       if this is a negative value, then the delay is infinite and the
       submenu will not be hidden until a different submenu is opened -->
  <showIcons>yes</showIcons>
  <!-- controls if icons appear in the client-list-(combined-)menu -->
  <manageDesktops>yes</manageDesktops>
  <!-- show the manage desktops section in the client-list-(combined-)menu -->
</menu>

<applications>
  <!-- Visit /etc/xdg/openbox/rc.xml for the complete list of properties -->
  <!-- Application specific keybinds can be implemented with an if action in the binding sections -->
  <application class="*">
    <decor>no</decor>
  </application>
  <application class="Ardour*">
    <decor>yes</decor>
  </application>
  <!-- position plasma osd -->
  <application name="plasmashell"
              class="plasmashell"
               type="normal">
    <position force="no">
      <x>center</x>
      <y>60%</y>
    </position>
    <desktop>all</desktop> <!-- 1 is the first desktop, 'all' for all desktops -->
  </application>
  <application name="wineconsole.exe"
               type="normal">
    <iconic>yes</iconic>
    <layer>below</layer>
  </application>
  <application name="megasync"
               title="MEGAsync"
               class="MEGAsync"
               type="normal">
    <iconic>yes</iconic>
  </application>
  <!-- In hope that Plank will stop showing a dock in the window list -->
  <application name="latte-dock"
               class="lattedock"
               type="dock">
    <skip_pager>yes</skip_pager>
    <skip_taskbar>yes</skip_taskbar>
  </application>
</applications>
</openbox_config>

◊; Local Variables:
◊; mode: pollen
◊; End:
◊; vim: filetype=pollen
