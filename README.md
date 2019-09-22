# System-Setup

Contains my setup for my computer

**Note: Package names are Fedora specific**

## Table of Contents

- [powertop2tuned](#powertop2tuned)
- [gpuselect](#gpuselect)
  - [Goal](#goal)
  - [How it works](#how-it-works)
  - [Usage](#Usage)
- [Editors](#Editors)
  - [System Dependencies](#System-Depedencies)
  - [Emacs](#Emacs)
  - [Sublime](#Sublime)
  - [List of Plugins](#List-of-Plugins)
- [Icons](#Icons)
- [Bluetooth Mouse Lag](#Bluetooth-Mouse-Lag)
- [Matlab](#Matlab)
- [Credits](#Credits)

## powertop2tuned

1. Download `powertop` and `tuned-utils`.

        sudo dnf install powertop tuned-utils

2. Create a powersaving profile for your laptop.

        sudo powertop2tuned -n laptop

3. Go to `/etc/tuned/laptop/tuned.conf`, this file contains commented lines for tweaking
   power usage. Uncommenting some lines will cause system instability, use at your own risk.

        sudo vi /etc/tuned/laptop/tuned.conf

4. For my specific case, I add the following lines before the line `[powertop_script]` in my `tuned.conf` file.

        # CPU energy performance preference
        /sys/devices/system/cpu/cpufreq/policy?/energy_performance_preference=balance_power

        [cpu]
        energy_perf_bias=power

        [usb]
        autosuspend=1

5. Enable your configuration and test it (i.e. restart, shutdown, sleep, run apps).

        sudo tuned-adm profile laptop
    
6. If there are no problems then allow tuned to run on system start.

        systemctl enable tuned

7. Create a udev rule to enable the laptop profile when the system is unplugged and to enable the 
   Desktop profile when the system is plugged in. I already created the udev rule in the repo, all 
   you need to do is copy `powersave.rules` into `/etc/udev/rules.d/`

        sudo cp powersave.rules /etc/udev/rules.d/

8. Restart your computer to see the results in powertop.

## gpuselect

### Goal
To imitate the function of prime-select in Ubuntu since prime-select doesn't
exist for Fedora. The gpu-select folder includes the `gpuselect.py`, used for the gpu switching, and
`gpuselect_test.py` used for testing `gpuselect.py`. The test cases are very limited, only testing
switching from nvidia to intel and intel to nvidia on a simple grub file. For complicated grub files
with out of order parameters in **GRUB_CMDLINE_LINUX=**, the script may not work.

### How it works
`gpuselect.py` reads the grub file in `/etc/default/grub`, and looks for 
"rd.driver.blacklist=nouveau modprobe.blacklist=nouveau nvidia-drm.modeset=1" substring in the
**GRUB_CMDLINE_LINUX=** parameters. If the selected gpu is intel and the substring is found,
then substring will be removed from the grub parameters. If the selected gpu is nvidia and the substring 
**is not** found then the substring will be inserted into the beginning of the grub parameter. Nothing
done in all other cases.

### Usage

1. Copy `gpuselect.py` to your PATH. In my case,

        sudo cp gpuselect.py /usr/local/bin

2. Whenever you need to switch the gpu.

        # To use integrated graphics
        sudo gpuselect.py intel
        # To use nvidia graphics
        sudo gpuselect.py nvidia

3. Restart computer to see changes

Note: This script only works for computers with optimus technology

## Editors

### System Dependencies

    # For irony-mode
    sudo dnf install gcc gcc-c++ make cmake clang-devel llvm-devel
    # For pdf-tools
    sudo dnf install automake autoconf gcc gcc-c++ ImageMagick libpng-devel zlib-devel poppler-glib-devel
    # For auctex
    sudo dnf install texlive-scheme-basic
    # For specific latex class files and stylesheets
    sudo dnf install 'tex(NAME.cls)' 
    sudo dnf install 'tex(NAME.sty)'    

### Emacs

1. Install system dependencies
2. Copy `Emacs/.emacs` file to `/home/$USER/` directory
3. Start Emacs

### Sublime

1. Install system dependencies
2. [Install Package Control](https://packagecontrol.io/installation)
3. Copy `Sublime/Users` folder to `/home/$USER/.config/sublime-text-3/Packages` directory
4. Start Sublime Text 3

### List of Plugins

- Emacs
  - Ease of Use
    - [multiple-cursor-mode](https://github.com/magnars/multiple-cursors.el), [vlf](https://github.com/m00natic/vlfi),
      [company](http://company-mode.github.io/), [yasnippet](https://github.com/joaotavora/yasnippet), 
      [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets), [helm](https://github.com/emacs-helm/helm)
  - Python
    - [anaconda-mode](https://github.com/proofit404/anaconda-mode), [company-anaconda](https://github.com/proofit404/company-anaconda)
  - C/C++
    - [irony-mode](https://github.com/Sarcasm/irony-mode), [company-irony](https://github.com/Sarcasm/company-irony), 
      [company-irony-c-headers](https://github.com/hotpxl/company-irony-c-headers)
  - HTML
    - [emmet-mode](https://github.com/smihica/emmet-mode)
      [impatient-mode](https://github.com/skeeto/impatient-mode), [company-web](https://github.com/osv/company-web)
  - Markdown
    - [markdown-mode](https://github.com/jrblevin/markdown-mode/tree/115f77df9755c6a453f3e5d9623ff885d207ea82)
  - Latex
    - [auctex](https://www.gnu.org/software/auctex/index.html)
  - pdf
    - [pdf-tools](https://github.com/politza/pdf-tools)
- Sublime
  - Ease of Use
    - [iOpener](https://github.com/rosshemsley/iOpener), [Terminus](https://github.com/randy3k/Terminus)
  - Python
    - [Anaconda](http://damnwidget.github.io/anaconda/)
  - Markdown
    - [MarkdownPreview](https://github.com/facelessuser/MarkdownPreview)

## Icons

icon.py is a script that creates icons for common apps I use. Icons are created in 
`$HOME/.local/share/applications/`

    # To add icon one by one and see each file contents
    python3 icon.py
    # For batch adding all icons at once
    python3 icon.py -a

## Bluetooth Mouse Lag

Copy config below into `/var/lib/bluetooth/XX:XX:XX:XX:XX:XX/ZZ:ZZ:ZZ:ZZ:ZZ:ZZ/info`
    
    [ConnectionParameters]
    MinInterval=6
    MaxInterval=9
    Latency=44
    Timeout=216
    
## Matlab

If matlab crashes when running `matlab -nosoftwareopengl`

    sudo mv /usr/local/MATLAB/R2019b/sys/os/glnxa64/libstdc++.so.6 /usr/local/MATLAB/R2019b/sys/os/glnxa64/libstdc++.so.6.old
    
## Credits

The powertop2tuned guide detailed above was based off [this reddit post](https://www.reddit.com/r/Fedora/comments/5pueys/how_to_save_power_with_your_laptop_running_fedora/). 
Most instructions are the same as the original poster, the difference is the udev rules file. 
The idea for `gpuselect.py` came from automating the instructions on the [RPM Fusion HowTo guide for Optimus](https://rpmfusion.org/Howto/NVIDIA?highlight=%28CategoryHowto%29#Optimus)
