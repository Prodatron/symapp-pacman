nolist

org #1000

WRITE "f:\symbos\apps\pacman.exe"
READ "..\..\..\..\SVN-Main\trunk\SymbOS-Constants.asm"

relocate_start

App_BegCode

;### APPLICATION HEADER #######################################################

;header structure
prgdatcod       equ 0           ;Length of the code area (OS will place this area everywhere)
prgdatdat       equ 2           ;Length of the data area (screen manager data; OS will place this area inside a 16k block of one 64K bank)
prgdattra       equ 4           ;Length of the transfer area (stack, message buffer, desktop manager data; placed between #c000 and #ffff of a 64K bank)
prgdatorg       equ 6           ;Original origin of the assembler code
prgdatrel       equ 8           ;Number of entries in the relocator table
prgdatstk       equ 10          ;Length of the stack in bytes
prgdatrs1       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10" SymbOS executable file identification
prgdatcex       equ 56          ;additional memory for code area (will be reserved directly behind the loaded code area)
prgdatdex       equ 58          ;additional memory for data area (see above)
prgdattex       equ 60          ;additional memory for transfer area (see above)
prgdatres       equ 62          ;*reserviert* (26 bytes)
prgdatver       equ 88          ;required OS version (1.0)
prgdatism       equ 90          ;Application icon (small version), 8x8 pixel, SymbOS graphic format
prgdatibg       equ 109         ;Application icon (big version), 24x24 pixel, SymbOS graphic format
prgdatlen       equ 256         ;length of header

prgpstdat       equ 6           ;start address of the data area
prgpsttra       equ 8           ;start address of the transfer area
prgpstspz       equ 10          ;additional sub process or timer IDs (4*1)
prgpstbnk       equ 14          ;64K ram bank (1-8), where the application is located
prgpstmem       equ 48          ;additional memory areas; 8 memory areas can be registered here, each entry consists of 5 bytes
                                ;00  1B  Ram bank number (1-8; if 0, the entry will be ignored)
                                ;01  1W  Address
                                ;03  1W  Length
prgpstnum       equ 88          ;Application ID
prgpstprz       equ 89          ;Main process ID

            dw App_BegData-App_BegCode  ;length of code area
            dw App_BegTrns-App_BegData  ;length of data area
            dw App_EndTrns-App_BegTrns  ;length of transfer area
prgdatadr   dw #1000                ;original origin                    POST address data area
prgtrnadr   dw relocate_count       ;number of relocator table entries  POST address transfer area
prgprztab   dw prgstk-App_BegTrns   ;stack length                       POST table processes
            dw 0                    ;*reserved*
App_BnkNum  db 0                    ;*reserved*                         POST bank number
            db "PacMan for SymbOS":ds 7:db 0   ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-App_BegCode ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-Kennung                 POST Tabelle Speicherbereiche
            dw fldxln*fldyln        ;zusätzlicher Code-Speicher
            dw fldxln*fldyln*8*2    ;zusätzlicher Data-Speicher
            dw 0                    ;zusätzlicher Transfer-Speicher
            ds 26                   ;*reserviert*
            db 0,3                  ;required OS version (4.0)

prgicnsml   db 2,8,8,#C3,#3C,#87,#1E,#2D,#0F,#0F,#F0,#1E,#F0,#0F,#0F,#87,#1E,#C3,#3C
prgicnbig   db 6,24,24
            db #F0,#F0,#C7,#3E,#F0,#F0,#F0,#E3,#0F,#0F,#7C,#F0,#F0,#87,#0F,#0F,#1E,#F0,#F0,#0F,#0F,#0F,#0F,#F8,#E1,#0F,#0F,#0F,#0F,#78,#C3,#0F,#0F,#0F,#0F,#3E,#C7,#0F,#0F,#0F,#0F,#1E,#87,#1E,#87,#0F,#0F,#78
            db #87,#1E,#87,#0F,#3C,#F0,#8F,#0F,#0F,#1E,#F0,#F0,#0F,#0F,#0F,#F0,#F0,#F0,#0F,#0F,#78,#90,#90,#90,#0F,#0F,#78,#90,#90,#90,#0F,#0F,#0F,#F0,#F0,#F0,#8F,#0F,#0F,#1E,#F0,#F0,#87,#0F,#0F,#0F,#3C,#F0
            db #87,#0F,#0F,#0F,#0F,#78,#C7,#0F,#0F,#0F,#0F,#1E,#C3,#0F,#0F,#0F,#0F,#3E,#E1,#0F,#0F,#0F,#0F,#78,#F0,#0F,#0F,#0F,#0F,#F8,#F0,#87,#0F,#0F,#1E,#F0,#F0,#E3,#0F,#0F,#7C,#F0,#F0,#F0,#C7,#3E,#F0,#F0


;*** SYSTEM MANAGER LIBRARY USAGE
use_SySystem_PRGRUN     equ 0   ;Starts an application or opens a document
use_SySystem_PRGEND     equ 0   ;Stops an application and frees its resources
use_SySystem_PRGSRV     equ 1   ;Manages shared services or finds applications
use_SySystem_SYSWRN     equ 0   ;Opens an info, warning or confirm box
use_SySystem_SELOPN     equ 0   ;Opens the file selection dialogue
use_SySystem_HLPOPN     equ 0   ;HLP file handling

;*** FILE MANAGER LIBRARY USAGE
use_SyFile_STOTRN       equ 0   ;Reads or writes a number of sectors
use_SyFile_FILNEW       equ 0   ;Creates a new file and opens it
use_SyFile_FILOPN       equ 1   ;Opens an existing file
use_SyFile_FILCLO       equ 1   ;Closes an opened file
use_SyFile_FILINP       equ 1   ;Reads an amount of bytes out of an opened file
use_SyFile_FILOUT       equ 0   ;Writes an amount of bytes into an opened file
use_SyFile_FILPOI       equ 1   ;Moves the file pointer to another position
use_SyFile_FILF2T       equ 0   ;Decodes the file timestamp
use_SyFile_FILT2F       equ 0   ;Encodes the file timestamp
use_SyFile_FILLIN       equ 0   ;Reads one text line out of an opened file
use_SyFile_FILCPR       equ 0   ;Reads (un)compressed data out of an opened file
use_SyFile_DIRDEV       equ 0   ;Sets the current drive
use_SyFile_DIRPTH       equ 0   ;Sets the current path
use_SyFile_DIRPRS       equ 0   ;Changes a property of a file or a directory
use_SyFile_DIRPRR       equ 0   ;Reads a property of a file or a directory
use_SyFile_DIRREN       equ 0   ;Renames a file or a directory
use_SyFile_DIRNEW       equ 0   ;Creates a new directory
use_SyFile_DIRINP       equ 0   ;Reads the content of a directory
use_SyFile_DIRDEL       equ 0   ;Deletes one or more files
use_SyFile_DIRRMD       equ 0   ;Deletes a sub directory
use_SyFile_DIRMOV       equ 0   ;Moves a file or sub directory
use_SyFile_DIRINF       equ 0   ;Returns information about one drive
use_SyFile_DEVDIR       equ 0   ;Reads the content of a directory (extended)

;*** SOUND DAEMON LIBRARY USAGE
use_SySound_RMTACT      equ 0   ;activates remote playing
use_SySound_RMTDCT      equ 0   ;deactivates remote playing
use_SySound_MUSLOD      equ 1   ;loads and inits music data
use_SySound_MUSFRE      equ 1   ;removes music data
use_SySound_MUSRST      equ 1   ;restarts a music
use_SySound_MUSCON      equ 0   ;continues playing a music
use_SySound_MUSSTP      equ 1   ;pauses and mutes music
use_SySound_MUSVOL      equ 1   ;sets music volume
use_SySound_EFXLOD      equ 1   ;loads and inits effect data
use_SySound_EFXFRE      equ 1   ;removes effect data
use_SySound_EFXPLY      equ 1   ;starts playing an effect
use_SySound_EFXSTP      equ 0   ;stop effects

READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-SystemManager.asm"
READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-FileManager.asm"
READ "..\..\..\..\SVN-Main\trunk\Docs-Developer\symbos_lib-SoundDaemon.asm"
READ "App-PacMan.asm"

App_EndTrns

relocate_table
relocate_end
