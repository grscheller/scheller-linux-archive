unset -f bash_initconf_ran

session=${2:-gnome}

case $session in
    xfce           ) exec startxfce4;;
    cinnamon       ) exec cinnamon-session;;
    gnome          ) exec gnome-session;;
    *              ) exec $2;;
esac
