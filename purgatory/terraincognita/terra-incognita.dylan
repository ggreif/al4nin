module: terra-incognita
synopsis: ICFP 2006 contest submission
author: heisenbug
copyright: � 2006 terraincognita team

define function main(name, arguments)
  format-out("Hello, world!\n");
  let um = make(<universal-machine>);
  um.spin-cycle;
  format-out("Done!\n");
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
