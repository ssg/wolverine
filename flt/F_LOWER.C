/*
F_LOWER
Wolverine filter example
Author: SSG
(converts all message to lowercase)
*/

#include <stdio.h>
#include <string.h>

void main(int argc, char *argv[])
{
  FILE *fi,*fo;
  char s[256];
  if (argc!=3) {
    printf("This filter requires Wolverine\n");
    return;
  }
  fi = fopen(argv[1],"r");
  if (fi==NULL) return;
  fo = fopen(argv[2],"w");
  if (fo==NULL) return;
  while (!feof(fi)) {
    fgets(s,256,fi);
    strlwr(s);
    fputs(s,fo);
  }
  fclose(fi);
  fclose(fo);
}
