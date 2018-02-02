#lang pollen
<?xml version="1.0" encoding="UTF-8"?>

<!-- Openbox config -->
<!-- I feel like I want to write this in some Xexp and compile it back to xml… -->

<!-- Documentation index <"http://openbox.org/wiki/Help:Contents"> -->
<!-- Configuration documentation <"http://openbox.org/wiki/Help:Configuration"> -->
<!-- Actions documentation <"http://openbox.org/wiki/Help:Actions"> -->
<!-- Keybinding documentation <"http://openbox.org/wiki/Help:Bindings"> -->

<openbox_config xmlns="http://openbox.org/3.4/rc"
                xmlns:xi="http://www.w3.org/2001/XInclude">

<resistance>
  <strength>10</strength>
  <screen_edge_strength>20</screen_edge_strength>
</resistance>

<focus>
  <focusNew>yes</focusNew> <!-- focus new windows when they appear -->
  <followMouse>no</followMouse> <!-- focus on hover -->
  <!-- followMouse settings -->
  <focusLast>yes</focusLast> <!-- on desktop switch, focus last used, not hovered -->
  <underMouse>no</underMouse> <!-- keep focus under mouse even when it's not moving -->
  <focusDelay>200</focusDelay> <!-- delay until focus change after hover -->
  <raiseOnFocus>no</raiseOnFocus> <!-- raise window if it gets focused -->
</focus>

<placement>
  <policy>Smart</policy> <!-- 'Smart' or 'UnderMouse' -->
  <center>yes</center> <!-- place windows in center (y) or top left (n) of free area -->
  <monitor>Primary</monitor> <!-- which monitor to place windows? 'Any' 'Mouse' 'Active' 'Primary' -->
  <primaryMonitor>1</primaryMonitor> <!-- which monitor is primary? 1-index or 'Mouse' 'Active' -->
</placement>

<theme>
  <name>Adapta-Nokto</name>
  <titleLayout>NLIMC</titleLayout>
  <!--
      available characters are NDSLIMC, each can occur at most once.
      N: window icon
      L: window label (AKA title).
      I: iconify
      M: maximize
      C: close
      S: shade (roll up/down)
      D: omnipresent (on all desktops).
  -->
  <keepBorder>yes</keepBorder>
  <animateIconify>no</animateIconify>
  ◊(font "ActiveWindow" "Roboto" 10 #:weight "normal" #:slant "normal")
  ◊(font "InactiveWindow" "Roboto" 10 #:weight "normal" #:slant "normal")
  ◊(font "MenuHeader" "sans" 9 #:weight "normal" #:slant "normal")
  ◊(font "MenuItem" "sans" 9 #:weight "normal" #:slant "normal")
  ◊(font "ActiveOnScreenDisplay" "sans" 9 #:weight "bold" #:slant "normal")
  ◊(font "InactiveOnScreenDisplay" "sans" 9 #:weight "bold" #:slant "normal")
</theme>

<desktops>
  <!-- this stuff is only used at startup, pagers allow you to change them
       during a session

       these are default values to use when other ones are not already set
       by other applications, or saved in your session

       use obconf if you want to change these without having to log out
       and back in -->
  <number>6</number>
  <firstdesk>1</firstdesk>
  <names>
    <!-- set names up here if you want to, like this:
    <name>desktop 1</name>
    <name>desktop 2</name>
    -->
  </names>
  <popupTime>0</popupTime>
  <!-- The number of milliseconds to show the popup for when switching
       desktops.  Set this to 0 to disable the popup. -->
</desktops>

<resize>
  <drawContents>yes</drawContents>
  <popupShow>Nonpixel</popupShow> <!-- 'Always' 'Never' 'Nonpixel' (like terminals) -->
  <popupPosition>Center</popupPosition> <!-- 'Center' 'Top' 'Fixed' -->
  <popupFixedPosition> <!-- coordinates when above is Fixed -->
    <!-- top left = 0,0; negative = from the other side -->
    <x>10</x> <!-- number or 'Center' -->
    <y>10</y> <!-- number or 'Center' -->
  </popupFixedPosition>
</resize>

<!-- You can reserve a portion of your screen where windows will not cover when
     they are maximized, or when they are initially placed.
     Many programs reserve space automatically, but you can use this in other
     cases. -->
<margins>
  <top>0</top>
  <bottom>0</bottom>
  <left>0</left>
  <right>0</right>
</margins>

<dock>
  <position>TopLeft</position>
  <!-- (Top|Bottom)(Left|Right|)|Top|Bottom|Left|Right|Floating -->
  <floatingX>0</floatingX>
  <floatingY>0</floatingY>
  <noStrut>no</noStrut>
  <stacking>Above</stacking> <!-- 'Above', 'Normal', or 'Below' -->
  <direction>Horizontal</direction> <!-- 'Vertical' or 'Horizontal' -->
  <autoHide>no</autoHide>
  <hideDelay>300</hideDelay> <!-- milliseconds -->
  <showDelay>300</showDelay> <!-- milliseconds -->
  <moveButton>Middle</moveButton> <!-- 'Left', 'Middle', 'Right' -->
</dock>

<!-- keybindings -->
<keyboard>
  <chainQuitKey>C-g</chainQuitKey>
  <keybind key="W-r">
    <action name="Execute"><command>raco pollen render ~/.config/openbox/</command></action>
    <action name="Execute"><command>openbox --reconfigure</command></action>
    ◊action/notify["Openbox --reconfigure complete" #:icon "view-refresh"]
  </keybind>

  <!-- Keybindings for desktop switching -->
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
  <keybind key="W-F10"><action name="ToggleDecorations"/></keybind>
  <keybind key="W-F11"><action name="ToggleFullscreen"/></keybind>
  <keybind key="W-Up W-k">
    ◊action/toggle-maximize-and-decorations[]
  </keybind>
  <keybind key="W-Down W-j">
    <!-- Minimize if not maximized -->
    <action name="if">
      <maximized>yes</maximized>
      <then>◊action/decorate-and-unmaximize[]</then>
      <else><action name="Iconify"/></else>
    </action>
  </keybind>

  <!-- Alt-tabbing -->
  <keybind key="A-Tab">
    <action name="NextWindow">
      <finalactions>
        <action name="Focus"/>
        <action name="Raise"/>
        <action name="Unshade"/>
      </finalactions>
    </action>
  </keybind>
  <keybind key="A-S-Tab">
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
  <keybind key="A-F1"> <!-- xcape handles W -> A-F1 -->
    <action name="Execute">
      <command>rofi -combi-modi window,drun,run -show combi</command>
    </action>
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

  <!-- one-off command stuff -->
  <!-- Restart PulseAudio with W-p -->
  <keybind key="W-p">
    <action name="Execute"><command>pulseaudio -k</command></action>
    <action name="Execute"><command>pulseaudio --start</command></action>
    <action name="Execute"><command>notify-send "PulseAudio has been restarted" " " --icon view-refresh</command></action>
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
  <keybind key="W-m" chroot="true">
    <keybind key="l Right">
      <action name="Execute"><command>xdotool key XF86AudioNext</command></action>
    </keybind>
    <keybind key="h Left">
      <action name="Execute"><command>xdotool key XF86AudioPrev</command></action>
    </keybind>
    <keybind key="p Home"> <!-- p for pause. bound to toggle pause/play in plasma. -->
      <action name="Execute"><command>notify-send "Media Controls" "Pause toggled" --icon player_pause</command></action>
      <action name="Execute"><command>xdotool key XF86AudioPlay</command></action>
    </keybind>
    <keybind key="s End"> <!-- s for stop -->
      <action name="Execute"><command>notify-send "Media Controls" "Audio stopped" --icon player_stop</command></action>
      <action name="Execute"><command>xdotool key XF86AudioStop</command></action>
    </keybind>
    <keybind key="j Down">
      <action name="Execute"><command>xdotool key XF86AudioLowerVolume</command></action>
    </keybind>
    <keybind key="k Up">
      <action name="Execute"><command>xdotool key XF86AudioRaiseVolume</command></action>
    <keybind key="S-j">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut decrease_microphone_volume</command></action>
    </keybind>
    <keybind key="S-k">
      <action name="Execute"><command>qdbus org.kde.kglobalaccel /component/kmix invokeShortcut increase_microphone_volume</command></action>
    </keybind>
  </keybind>

  <!-- Keybindings for running applications -->

  <!-- keymap -->
  <keybind key="W-o">
    <action name="Execute"><command>bash -c 'xmodmap &lt;(cat ~/.Xmodmap ~/.Xmodmap.d/osu.xmodmap)'</command></action>
    <action name="Execute"><command>wacom-sensitivity -c -s 450</command></action>
    <action name="Execute"><command>notify-send "Using osu! keymap" " "</command></action>
  </keybind>
  <keybind key="W-S-o">
    <action name="Execute"><command>bash -c 'xmodmap &lt;(cat ~/.Xmodmap)'</command></action>
    <action name="Execute"><command>wacom-sensitivity -c -s 950</command></action>
    <action name="Execute"><command>notify-send "Using default keymap" " "</command></action>
  </keybind>
  <!-- keybinds for osu!. use a keymap to bind stuff to F32-34 -->
  <keybind key="F31">
    <action name="Execute"><command>xdotool key F2</command></action>
  </keybind>
  <keybind key="F32">
    <action name="Execute"><command>xdotool key Escape</command></action>
  </keybind>
  <keybind key="F33">
    <action name="Execute"><command>xdotool key Tab</command></action>
  </keybind>
  <keybind key="F34">
    <action name="Execute"><command>bash -c "xdotool keydown grave; sleep 0.7; xdotool keyup grave"</command></action>
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
    <mousebind button="W-Left" action="Press">
      <action name="Focus"/>
      <action name="Raise"/>
    </mousebind>
    <mousebind button="W-Left" action="Click">
      <action name="Unshade"/>
    </mousebind>
    <mousebind button="W-Left" action="Drag">
      <action name="if">
        <maximized>yes</maximized>
        <then>
          ◊action/decorate-and-unmaximize[]
        </then>
      </action>
      <action name="Move"/>
    </mousebind>

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
      <action name="if">
        <maximized>yes</maximized>
        <then>
          ◊action/decorate-and-unmaximize[]
        </then>
      </action>
      <action name="Move"/>
    </mousebind>
    <mousebind button="Left" action="DoubleClick">
      ◊action/toggle-maximize-and-decorations[]
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
      ◊action/undecorate-and-maximize[]
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
</applications>
</openbox_config>

◊; Local Variables:
◊; mode: pollen
◊; End:
