# Archaic

Arch Automatic Installation Commands

(Still somewhat personal)

## What it does

- Main Arch Linux on top of LVM on top of LUKS. Bare minimum desktop with XMonad+xmobar.
- Secondary minimal Arch Linux as a rescue alternative.
- refind boot manager.

## Requirements

- UEFI system.
- Secure Boot disabled from UEFI.
- GPT disk.
- Existing EFI partition (only tested with the one created by Windows 10 in a Lenovo Thinkpad)
- Free un-partitioned disk space ready to be asigned according to instructions. See comments inside `run_archaic`.
- Live Arch Installation ISO (only tested with a USB stick on March 2018).
- Internet conection.

**IMPORTANT:** There are many more requirements and constraints that you will only understand reading `run_archaic` and `_inside_arch_chroot`.

## Usage

- Put a copy of this repo inside your **running** Live Arch Installation ISO.
- Open `run_archaic` in a text editor, read it, check it, double-check it and edit the required variables. Left `$debug=` commented, so you can do a dry-run.
- Run `run_archaic` script. It should be in **debug** mode so you can check that all text replacements went fine.
- Re-open `run_archaic` and uncomment the `debug=` line.
- Run `run_archaic` script and wait for input prompts.
- Do not relax too much.

If you need to modify the code, remember: [shellcheck](https://github.com/koalaman/shellcheck) is your friend.

## In case of errors

To restart the process, remember to:
- delete `/boot/EFI/refind/`
- delete all kernels and initramfs in `/boot/`

## Inspiration

- https://shirotech.com/linux/how-to-automate-arch-linux-installation

## Wanna help?

- Better diagnose of errors.
- Smarter pacman repository to avoid redownloading packages.
- Remove lot of duplicate and ugly bash code.
- Ease testing with virtualization.
- Suspend to RAM vs. crypt. What to do?
- Many steps should be optional: backups, secondary_arch, etc.
- Many hardcoded options: keyboard layout, timedate locale, etc.
