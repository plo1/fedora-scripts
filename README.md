# Fedora-Scripts
Contains my setup for controlling nVidia Optimus and instruction for enabling powersaving features.

## Dependencies
- powertop2tuned
    - `powertop`
    - `tuned-utils`
- gpuselect
    - `python3`

## powertop2tuned
1. Download `powertop` and `tuned-utils`.

    `sudo dnf install powertop tuned-utils`

2. Create a powersaving profile for your laptop.
    
    `sudo powertop2tuned -n laptop`

3. Go to `/etc/tuned/laptop/tuned.conf`, this file contains commented lines for tweaking
   power usage. Uncommenting some lines will cause system instability, use at your own risk.

    `sudo vi /etc/tuned/laptop/tuned.conf`

4. For my specific case, I add the following lines before the line `[powertop_script]` in my `tuned.conf` file.
    
        # CPU energy performance preference
        /sys/devices/system/cpu/cpufreq/policy?/energy_performance_preference=balance_power

        [cpu]
        energy_perf_bias=power

        [usb]
        autosuspend=1
        
5. Enable your configuration and test it (i.e. restart, shutdown, sleep, run apps).

    `sudo tuned-adm profile laptop`
    
6. If there are no problems then allow tuned to run on system start.
   
    `systemctl enable tuned`

7. Create a udev rule to enable the laptop profile when the system is unplugged and to enable the 
   Desktop profile when the system is plugged in. I already created the udev rule in the repo, all 
   you need to do is copy `powersave.rules` into `/etc/udev/rules.d/`

    `sudo cp powersave.rules /etc/udev/rules.d/`

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

### How to use
1. Copy `gpuselect.py` to your PATH. In my case,

    `sudo cp gpuselect.py /usr/local/bin`

2. Whenever you need to switch the gpu.

        # To use integrated graphics
        sudo gpuselect.py intel

        # To use nvidia graphics
        sudo gpuselect.py nvidia

3. Restart computer to see changes

Note: This script only works for computers with optimus technology

## Credits
The powertop2tuned guide detailed above was based off [this reddit post](https://www.reddit.com/r/Fedora/comments/5pueys/how_to_save_power_with_your_laptop_running_fedora/). 
Most instructions are the same as the original poster, the difference is the udev rules file. 
The idea for `gpuselect.py` came from automating the instructions on the [RPM Fusion HowTo guide for Optimus](https://rpmfusion.org/Howto/NVIDIA?highlight=%28CategoryHowto%29#Optimus)
