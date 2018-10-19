unset -f bash_initconf_ran

session=${2:-gnome}

case $session in
    gnome          ) exec gnome-session;;
    xfce           ) exec startxfce4;;
    cinnamon       ) exec cinnamon-session;;
    *              ) exec $2;;
esac
