; 10 SYS (4096)
*=$0801
          BYTE           $0E, $08, $0A, $00, $9E, $20, $28,  $34, $30, $39, $36, $29, $00, $00, $00
*=$1000
;helpful labels
CLEAR     = $E544
EXTCOL    = $D020       ; Border Color
GETIN     = $FFE4
CHROUT    = $FFD2
SCNKEY    = $FF9F
PLOT      = $E50A
STROUT    = $AB1E
LINPRT    = $BDCD
PLAYER_CHR= #88
PLAYER_ROW_I = $02
PLAYER_COL_I = $03
PLAYER_PD  = $08         ; player prev dir (1 up, 2 left, 3 right, 4 down)
rowL = $04
rowH = $05
scoreL = $09
scoreH = $0A
resultL = $0B
resultH = $0C
GAMESPEED = #80
GHOST1_PREV_MOVE = $17
GHOST1_ND = $19
GHOST1_ROW_I = $21
GHOST1_COL_I = $23
Z1 = $0F
ghost_moves = $10
GHOST_CHR = #65
WALL      = #102
DOT       = #46

CHAR_U = #85
CHAR_D = #68
CHAR_L = #76
CHAR_R = #82


start
          lda #03
          sta PLAYER_PD
          lda #00
          sta scoreL
          sta scoreH
          sta ghost_moves
          lda #13
          sta PLAYER_COL_I
          lda #11
          sta PLAYER_ROW_I
          lda #01
          sta GHOST1_COL_I
          lda #01
          sta GHOST1_ROW_I
          lda #04
          sta GHOST1_PREV_MOVE
          lda #5
          sta EXTCOL
          jsr drwscrn
          jsr DRWGHOST1
          clc
gameloop
          jsr CLRGHOST1
          jsr MOVEGHOSTS
          jsr DELAY
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

          LDA PLAYER_PD
          CMP #01
          BEQ JMP_MVPLYR_UP
          CMP #02
          BEQ JMP_MVPLYR_LF
          CMP #03
          BEQ JMP_MVPLYR_RT
          CMP #04
          BEQ JMP_MVPLYR_DN

          jmp gameloop

END       
          JSR CLEAR
          RTS

JMP_MVPLYR_UP
          JMP MVPLYR_UP
JMP_MVPLYR_DN
          JMP MVPLYR_DN
JMP_MVPLYR_LF
          JMP MVPLYR_LF
JMP_MVPLYR_RT
          JMP MVPLYR_RT

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
DRWPLYR   
          ldx PLAYER_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          lda GHOST_CHR
          cmp (rowL),Y
          bne DRWPLYR_OK
          jmp GHOST_COLL
DRWPLYR_OK
          lda PLAYER_CHR
          sta (rowL),Y
          ldx PLAYER_ROW_I
          lda ColorRowTableDataL,x
          sta rowL
          lda ColorRowTableDataH,x
          sta rowH
          ldy PLAYER_COL_I
          lda #1
          sta (rowL),Y
          rts
GHOST_COLL
          lda #2
          sta EXTCOL
          jmp GHOST_COLL

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
          ;JSR LINPRT          
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
          JMP gameloop
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
          sta PLAYER_PD
          jsr DRWPLYR
          jmp gameloop
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
          JMP gameloop
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
          sta PLAYER_PD
          jsr DRWPLYR
          jmp gameloop
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
          JMP gameloop
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
          sta PLAYER_PD
          jsr DRWPLYR
          jmp gameloop
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
          JMP gameloop
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
          sta PLAYER_PD
          jsr DRWPLYR
          jmp gameloop
MOVEGHOSTS
          clc
          lda #0
          sta Z1
          ldy #0
          lda #46
          sta TXTSA,Y
          iny
          sta TXTSA,Y
          iny
          sta TXTSA,Y
          iny
          sta TXTSA,Y
GHOST1_CHK_UP          ; can ghost move up?
          lda #04
          cmp GHOST1_PREV_MOVE
          beq GHOST1_CHK_DN
          ldx GHOST1_ROW_I
          dex
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          lda WALL
          cmp (rowL),Y
          beq GHOST1_CHK_DN   
          LDY Z1
          LDA CHAR_U
          STA TXTSA,Y
          iny
          sty Z1
GHOST1_CHK_DN
          lda #01
          cmp GHOST1_PREV_MOVE
          beq GHOST1_CHK_LF
          ldx GHOST1_ROW_I
          inx
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          lda WALL
          cmp (rowL),Y
          beq GHOST1_CHK_LF               
          LDY Z1
          LDA CHAR_D
          STA TXTSA,Y
          iny
          sty Z1
GHOST1_CHK_LF
          lda #03
          cmp GHOST1_PREV_MOVE
          beq GHOST1_CHK_RT
          ldx GHOST1_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          dey
          lda WALL
          cmp (rowL),Y
          beq GHOST1_CHK_RT               
          LDY Z1
          LDA CHAR_L
          STA TXTSA,Y
          iny
          sty Z1
GHOST1_CHK_RT
          lda #02
          cmp GHOST1_PREV_MOVE
          beq MVGHOST1
          ldx GHOST1_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          iny
          lda WALL
          cmp (rowL),Y
          beq MVGHOST1 
          LDY Z1
          LDA CHAR_R
          STA TXTSA,Y
          iny
          sty Z1
MVGHOST1
          jsr GHOSTDEBUG
          ;JSR SCNKEY    ; get key
          ;JSR GETIN     ; put key in A
          ;CMP #67
          ;BNE MVGHOST1
          ldy #0
          ldx TXTSA,Y
          txa
          cmp CHAR_U
          beq MVGHOST1_UP
          txa
          cmp CHAR_D
          beq MVGHOST1_DN
          txa
          cmp CHAR_L
          beq MVGHOST1_LT
          txa
          cmp CHAR_R
          beq MVGHOST1_RT
MVGHOST1_UP
          lda GHOST1_ROW_I
          sec
          sbc #1
          sta GHOST1_ROW_I
          lda #01
          sta GHOST1_PREV_MOVE
          jmp DRWGHOST1
MVGHOST1_DN
          lda GHOST1_ROW_I
          clc
          adc #1
          sta GHOST1_ROW_I
          lda #04
          sta GHOST1_PREV_MOVE
          jmp DRWGHOST1
MVGHOST1_LT
          lda GHOST1_COL_I
          sec
          sbc #1
          sta GHOST1_COL_I
          lda #02
          sta GHOST1_PREV_MOVE
          jmp DRWGHOST1
MVGHOST1_RT
          lda GHOST1_COL_I
          clc
          adc #1
          sta GHOST1_COL_I
          lda #03
          sta GHOST1_PREV_MOVE

DRWGHOST1
          ldx GHOST1_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          lda GHOST_CHR
          sta (rowL),Y
          ldx GHOST1_ROW_I
          lda ColorRowTableDataL,x
          sta rowL
          lda ColorRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          lda #7
          sta (rowL),Y
          rts
CLRGHOST1
          ldx GHOST1_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOST1_COL_I
          lda #32
          sta (rowL),Y
          rts
GHOSTDEBUG
          clc
          ldy #30
          ldx #5
          jsr PLOT
          ldy #0
          lda TXTSA,Y
          jsr CHROUT
          iny
          lda TXTSA,Y
          jsr CHROUT
          iny
          lda TXTSA,Y
          jsr CHROUT
          iny
          lda TXTSA,Y
          jsr CHROUT
          rts
drwscrn
          incasm "screen.asm"

TXTSA  BYTE $00,$00,$00,$00