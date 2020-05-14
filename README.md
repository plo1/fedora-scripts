# System-Setup

Contains my setup for my computer

**Note: Package names are Ubuntu 18.04.4 LTS specific**

## Table of Contents

- [powertop2tuned](#powertop2tuned)
- [Editors](#Editors)
  - [System Dependencies](#System-Dependencies)
  - [Emacs](#Emacs)
  - [Sublime](#Sublime)
  - [List of Plugins](#List-of-Plugins)
- [Icons](#Icons)
- [Bluetooth Mouse Lag](#Bluetooth-Mouse-Lag)
- [Matlab](#Matlab)
- [GPU Passthrough](#GPU-Passthrough)
- [GDM dual monitor setup](#Dual-Monitor)
- [KVM filesharing](#KVM-filesharing)
- [CUDA Path](#CUDA-Path)
- [Credits](#Credits)

## powertop2tuned

1. Download `powertop`, `tuned-utils`, and `linux-tools-generic-hwe-20.04`.

        sudo apt install linux-tools-generic-hwe-20.04 powertop tuned-utils

2. Create a powersaving profile for your laptop.

        sudo powertop2tuned -n laptop

3. Go to `/etc/tuned/laptop/tuned.conf`, this file contains commented lines for tweaking
   power usage. Uncommenting some lines will cause system instability, use at your own risk.

        sudo vi /etc/tuned/laptop/tuned.conf

4. For my specific case, I add the following lines before the line `[powertop_script]` in my `tuned.conf` file.

        # CPU energy performance preference
        /sys/devices/system/cpu/cpu?/cpufreq/energy_performance_preference=balance_power

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

## Editors

### System Dependencies

    # For C/C++ completions
    sudo apt install clangd
    # For LATEX
    sudo apt install texlive-full
    # For LATEX completions
    sudo apt install lua5.3 luarocks liblua5.3-dev
    sudo luarocks install digestif
    # For Markdown preview
    sudo apt install discount
    # For python language server
    sudo apt install python3-pip
    pip3 install 'python-language-server[all]'

### Emacs

1. Install system dependencies
2. Copy `Emacs/.emacs` file to `/home/$USER/` directory
3. Start Emacs

Note: if there is a gpg key issue with elpa run `gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40`

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
      [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets), [helm](https://github.com/emacs-helm/helm),
      [lsp-mode](https://github.com/emacs-lsp/lsp-mode#installation), [company-lsp](https://github.com/tigersoldier/company-lsp),
  - Python
    - [anaconda-mode](https://github.com/proofit404/anaconda-mode), [company-anaconda](https://github.com/proofit404/company-anaconda)
  - C/C++
    - [irony](https://github.com/Sarcasm/irony-mode), [company-irony](https://github.com/Sarcasm/company-irony), 
      [company-irony-c-headers](https://github.com/hotpxl/company-irony-c-headers), [irony-eldoc](https://github.com/ikirill/irony-eldoc)
  - Java
    - [lsp-java](https://github.com/emacs-lsp/lsp-java), [lsp-mode](https://github.com/emacs-lsp/lsp-mode#installation)
  - HTML
    - [emmet-mode](https://github.com/smihica/emmet-mode)
      [impatient-mode](https://github.com/skeeto/impatient-mode), [company-web](https://github.com/osv/company-web)
  - Markdown
    - [markdown-mode](https://github.com/jrblevin/markdown-mode/tree/115f77df9755c6a453f3e5d9623ff885d207ea82)
  - Latex
    - [auctex](https://www.gnu.org/software/auctex/index.html), [lsp-mode](https://github.com/emacs-lsp/lsp-mode#installation)
  - pdf
    - [pdf-tools](https://github.com/politza/pdf-tools)
- Sublime
  - Ease of Use
    - [iOpener](https://github.com/rosshemsley/iOpener), [Terminus](https://github.com/randy3k/Terminus)
  - Python
    - [Anaconda](http://damnwidget.github.io/anaconda/)
  - Markdown
    - [MarkdownPreview](https://github.com/facelessuser/MarkdownPreview)
  - Latex
    - [LaTeXTools](https://packagecontrol.io/packages/LaTeXTools)
  - C/C++
    - [EasyClangComplete](https://packagecontrol.io/packages/EasyClangComplete)

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
    
## GPU Passthrough

For Enabling IOMMU
1. Append `intel_iommu=on iommu=pt rd.driver.pre=vfio-pci` to /etc/default/grub then restart
2. Run `iommu_groups.sh` to check id of passthrough devices
3. Create `/etc/modprobe.d/vfio.conf` then type `options vfio-pci ids=ID_1,ID2,...ID_N` into the file where each id is the passthrough devices'
4. Create `/etc/dracut.conf.d/vfio.conf` then type `add_drivers+="vfio vfio_iommu_type1 vfio_pci vfio_virqfd"` into the file
5. Run ``sudo dracut -f --kver `uname -r` ``
6. Run `sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg`
7. Restart and check device has been added by running `dmesg | grep -i vfio`

For configuration of Win10 VM

Add following to `virsh edit win10`
    
    <features>
        ...
        <kvm>
            <hidden state='on'/>
        </kvm>
        ...
    </features>
    <cpu mode='host-passthrough' check='none'>
        <topology sockets='1' cores='4' threads='1'/>
    </cpu>

Sourced from https://level1techs.com/article/ryzen-gpu-passthrough-setup-guide-fedora-26-windows-gaming-linux
And https://wiki.archlinux.org/index.php/PCI_passthrough_via_OVMF
And https://qubitrenegade.com/virtualization/kvm/vfio/2019/07/17/VFIO-Fedora-Notes.html

## Dual Monitor

If gnome login screen is on the wrong monitor, configure properly in gnome-control panel then

`sudo cp .config/monitors.xml ~gdm/.config/monitors.xml`

Restart gdm or computer

## KVM filesharing

Create a folder you want to share and give it permission

    user@host$ mkdir test_folder
    user@host$ chmod 777 test_folder
    
In virt-manager when adding a filesystem make sure settings are as follows

    Type: mount
    Driver: default
    Mode: Mapped
    Source path: /path_to_folder/test_folder
    Target path: share

To test out folder mount in guest system
    
    user@guest$ mkdir mnt
    user@guest# mount -t 9p -o trans=virtio,version=9p2000.L,rw share /path_to_mnt/mnt

To mount folder in guest at boot add `share   /path_to_mntpt/mnt    9p  trans=virtio,version=9p2000.L,rw    0   0` to `/etc/fstab` of guest system

If on Ubuntu 18.04 LTS or below, add to `/etc/initramfs-tools/modules`

    9p
    9pnet
    9pnet_virtio
    
Then:
    
    sudo update-initramfs -u
    
## CUDA Path

Install [CUDA](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html#post-installation-actions) and [cudnn](https://docs.nvidia.com/deeplearning/sdk/cudnn-install/index.html#install-linux) then follow setting up environmental variable section in [Puget Systems Tutorial](https://www.pugetsystems.com/labs/hpc/How-to-install-CUDA-9-2-on-Ubuntu-18-04-1184/)


## Credits

The powertop2tuned guide detailed above was based off [this reddit post](https://www.reddit.com/r/Fedora/comments/5pueys/how_to_save_power_with_your_laptop_running_fedora/). 
Most instructions are the same as the original poster, the difference is the udev rules file. 
The idea for `gpuselect.py` came from automating the instructions on the [RPM Fusion HowTo guide for Optimus](https://rpmfusion.org/Howto/NVIDIA?highlight=%28CategoryHowto%29#Optimus)
