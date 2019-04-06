; 10 SYS (4096)
*=$0801
          BYTE           $0E, $08, $0A, $00, $9E, $20, $28,  $34, $30, $39, $36, $29, $00, $00, $00
*=$1000
;helpful labels
CLEAR     = $E544
EXTCOL    = $D020
GETIN     = $FFE4
CHROUT    = $FFD2
SCNKEY    = $FF9F
PLOT      = $E50A
STROUT    = $AB1E
LINPRT    = $BDCD
PLAYER    = #88
PLAYER_ROW_I = $02
PLAYER_COL_I = $03
PLAYERPD  = $00         ; player prev dir (1 up, 2 left, 3 right, 4 down)
rowL = $04
rowH = $05
scoreL = $09
scoreH = $0A
resultL = $0B
resultH = $0C
GAMESPEED = #100
GHOSTX    = #01
GHOSTY    = #01
SCRN_START = $0400
;SCRN_OFFSET = $03
WALL      = #102
DOT       = #46

start
          lda #00
          sta PLAYERPD
          sta scoreL
          sta scoreH
          lda #13
          sta PLAYER_COL_I
          lda #11
          sta PLAYER_ROW_I
          ;lda #252
          ;jsr EXTCOL
          jsr drwscrn
          clc
DRWPLYR   
          ldx PLAYER_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          lda PLAYER
          sta (rowL),Y
SCAN      
          JSR SCNKEY    ; get key
          JSR GETIN     ; put key in A
          CMP #87
          BEQ JMP_MVPLYR_UP
          CMP #83
          BEQ JMP_MVPLYR_DN
          CMP #65
          BEQ JMP_MVPLYR_LF
          CMP #68
          BEQ JMP_MVPLYR_RT
          CMP #81       ; end if Q clicked
          BEQ END

          LDA PLAYERPD
          CMP #01
          BEQ JMP_MVPLYR_UP
          CMP #02
          BEQ JMP_MVPLYR_LF
          CMP #03
          BEQ JMP_MVPLYR_RT
          CMP #04
          BEQ JMP_MVPLYR_DN
          JMP SCAN
JMP_MVPLYR_UP
          JMP MVPLYR_UP
JMP_MVPLYR_DN
          JMP MVPLYR_DN
JMP_MVPLYR_LF
          JMP MVPLYR_LF
JMP_MVPLYR_RT
          JMP MVPLYR_RT
END       
          JSR CLEAR
          RTS

CLRPLYR   
          ldx PLAYER_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          lda #32
          sta (rowL),Y
          rts
DELAY
          LDA #$00
          STA $BB
          STA $BC 
DLY
          INC $BB
          BNE DLY 
          INC $BC
          LDA $BC
          CMP GAMESPEED
          BNE DLY 
          RTS     
INCSCORE
          clc
          ldy #30
          ldx #2
          jsr PLOT
          CLC
          lda scoreL
          adc #05
          sta resultL       ; store sum of LSBs
          tax
          stx scoreL
          lda scoreH
          adc #00           ; add the MSBs using carry from
          sta resultH       ; the previous calculation
          tax
          stx scoreH
          LDX scoreL 
          LDA scoreH 
          JSR LINPRT          
          RTS
MVPLYR_UP
          ; OK move up ?
          ldx PLAYER_ROW_I
          dex
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          lda WALL
          cmp (rowL),Y
          bne MVPLYR_UP_OK               
          JMP SCAN
MVPLYR_UP_OK
          jsr DELAY
          lda DOT
          cmp (rowL),Y
          bne MVPLYR_UP_OK_CONT
          jsr INCSCORE
MVPLYR_UP_OK_CONT
          jsr CLRPLYR
          lda PLAYER_ROW_I
          sec
          sbc #1
          sta PLAYER_ROW_I
          lda #01
          sta PLAYERPD
          JMP DRWPLYR
MVPLYR_DN
          ; OK move down ?
          ldx PLAYER_ROW_I
          inx
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          lda WALL
          cmp (rowL),Y
          bne MVPLYR_DN_OK               
          JMP SCAN
MVPLYR_DN_OK
          jsr DELAY
          lda DOT
          cmp (rowL),Y
          bne MVPLYR_DN_OK_CONT
          jsr INCSCORE
MVPLYR_DN_OK_CONT
          jsr CLRPLYR
          lda PLAYER_ROW_I
          clc
          adc #1
          sta PLAYER_ROW_I
          lda #04
          sta PLAYERPD
          JMP DRWPLYR
MVPLYR_LF
          ; OK move left ?
          ldx PLAYER_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          dey
          lda WALL
          cmp (rowL),Y
          bne MVPLYR_LF_OK               
          JMP SCAN
MVPLYR_LF_OK
          jsr DELAY
          lda DOT
          cmp (rowL),Y
          bne MVPLYR_LF_OK_CONT
          jsr INCSCORE
MVPLYR_LF_OK_CONT
          jsr CLRPLYR
          lda PLAYER_COL_I
          sec
          sbc #1
          sta PLAYER_COL_I
          lda #02
          sta PLAYERPD
          JMP DRWPLYR
MVPLYR_RT
          ; OK move right ?
          ldx PLAYER_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          iny
          lda WALL
          cmp (rowL),Y
          bne MVPLYR_RT_OK 
          JMP SCAN
MVPLYR_RT_OK
          jsr DELAY
          lda DOT
          cmp (rowL),Y
          bne MVPLYR_RT_OK_CONT
          jsr INCSCORE
MVPLYR_RT_OK_CONT
          jsr CLRPLYR
          lda PLAYER_COL_I
          clc
          adc #1
          sta PLAYER_COL_I
          lda #03
          sta PLAYERPD
          JMP DRWPLYR
SCRNROWS  
          byte $0400
drwscrn
          incasm "screen.asm"