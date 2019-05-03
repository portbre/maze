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

FREHI3 = $D40F
VCREG3 = $D412
SIGVOL = $D418
RANDOM = $D41B

PLAYER_ROW_I = $02
PLAYER_COL_I = $03
PLAYER_PD  = $08         ; player prev dir (1 up, 2 left, 3 right, 4 down)
rowL = $04
rowH = $05
scoreL = $09
scoreH = $0A
resultL = $0B
resultH = $0C
Z1 = $0F
;ghost_moves = $10
ACTIVE_GHOST = $81

GAMESPEED = #40
GHOST1_PREV_MOVE = $17
GHOST1_ND = $19
GHOST1_ROW_I = $21
GHOST1_COL_I = $23

GHOST2_PREV_MOVE = $24
GHOST2_ND = $25
GHOST2_ROW_I = $26
GHOST2_COL_I = $27

GHOST3_PREV_MOVE = $28
GHOST3_ND = $29
GHOST3_ROW_I = $2A
GHOST3_COL_I = $2B

GHOST4_PREV_MOVE = $2C
GHOST4_ND = $2D
GHOST4_ROW_I = $2E
GHOST4_COL_I = $2F

GHOSTX_PREV_MOVE = $30
GHOSTX_ND = $31
GHOSTX_ROW_I = $32
GHOSTX_COL_I = $33


GHOST_CHR = #65
PLAYER_CHR= #88
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
          ;sta ghost_moves
          sta ACTIVE_GHOST

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

          lda #01
          sta GHOST2_COL_I
          lda #23
          sta GHOST2_ROW_I
          lda #04
          sta GHOST2_PREV_MOVE

          lda #25
          sta GHOST3_COL_I
          lda #01
          sta GHOST3_ROW_I
          lda #04
          sta GHOST3_PREV_MOVE

          lda #25
          sta GHOST4_COL_I
          lda #23
          sta GHOST4_ROW_I
          lda #04
          sta GHOST4_PREV_MOVE

          lda #5
          sta EXTCOL
          JSR RDINIT
          jsr drwscrn
          ;jsr DRWGHOST
          clc
gameloop
          ;jsr CLRGHOST
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

RDINIT
          LDA #$FF
          STA FREHI3
          LDA #%10000000
          STA VCREG3
          STA SIGVOL
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

          INC ACTIVE_GHOST
          LDA ACTIVE_GHOST
          CMP #01
          BEQ SET_GHOST1
          CMP #02
          BEQ SET_GHOST2
          CMP #03
          BEQ SET_GHOST3
          CMP #04
          BEQ SET_GHOST4
          
          lda #0
          sta ACTIVE_GHOST
          rts

SET_GHOST1
          lda GHOST1_PREV_MOVE
          sta GHOSTX_PREV_MOVE
          lda GHOST1_ROW_I
          sta GHOSTX_ROW_I
          lda GHOST1_COL_I
          sta GHOSTX_COL_I
          jsr CLRGHOST
          jmp GHOSTX_CHK_UP

SET_GHOST2
          lda GHOST2_PREV_MOVE
          sta GHOSTX_PREV_MOVE
          lda GHOST2_ROW_I
          sta GHOSTX_ROW_I
          lda GHOST2_COL_I
          sta GHOSTX_COL_I
          jsr CLRGHOST
          jmp GHOSTX_CHK_UP

SET_GHOST3
          lda GHOST3_PREV_MOVE
          sta GHOSTX_PREV_MOVE
          lda GHOST3_ROW_I
          sta GHOSTX_ROW_I
          lda GHOST3_COL_I
          sta GHOSTX_COL_I
          jsr CLRGHOST
          jmp GHOSTX_CHK_UP

SET_GHOST4
          lda GHOST4_PREV_MOVE
          sta GHOSTX_PREV_MOVE
          lda GHOST4_ROW_I
          sta GHOSTX_ROW_I
          lda GHOST4_COL_I
          sta GHOSTX_COL_I
          jsr CLRGHOST
          jmp GHOSTX_CHK_UP

GHOSTX_CHK_UP          ; can ghost move up?
          lda #04
          cmp GHOSTX_PREV_MOVE
          beq GHOSTX_CHK_DN
          ldx GHOSTX_ROW_I
          dex
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
          lda WALL
          cmp (rowL),Y
          beq GHOSTX_CHK_DN   
          LDY Z1
          LDA CHAR_U
          STA TXTSA,Y
          iny
          sty Z1
GHOSTX_CHK_DN
          lda #01
          cmp GHOSTX_PREV_MOVE
          beq GHOSTX_CHK_LF
          ldx GHOSTX_ROW_I
          inx
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
          lda WALL
          cmp (rowL),Y
          beq GHOSTX_CHK_LF               
          LDY Z1
          LDA CHAR_D
          STA TXTSA,Y
          iny
          sty Z1
GHOSTX_CHK_LF
          lda #03
          cmp GHOSTX_PREV_MOVE
          beq GHOSTX_CHK_RT
          ldx GHOSTX_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
          dey
          lda WALL
          cmp (rowL),Y
          beq GHOSTX_CHK_RT               
          LDY Z1
          LDA CHAR_L
          STA TXTSA,Y
          iny
          sty Z1
GHOSTX_CHK_RT
          lda #02
          cmp GHOSTX_PREV_MOVE
          beq MVGHOSTX
          ldx GHOSTX_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
          iny
          lda WALL
          cmp (rowL),Y
          beq MVGHOSTX 
          LDY Z1
          LDA CHAR_R
          STA TXTSA,Y
          iny
          sty Z1
MVGHOSTX
          ;jsr GHOSTDEBUG
          lda Z1
          cmp #01
          beq MVGHOSTX_RND_1
          cmp #02
          beq MVGHOSTX_RND_2

MVGHOSTX_RND_3
          ;lda #2
          LDA RANDOM
          cmp #85
          bcc MVGHOSTX_RND_3_1
          cmp #170
          bcc MVGHOSTX_RND_3_2        
          lda #2
          jmp MVGHOSTX_RND_3_CONT

MVGHOSTX_RND_3_1
          lda #0
          jmp MVGHOSTX_RND_3_CONT
MVGHOSTX_RND_3_2
          lda #1
          jmp MVGHOSTX_RND_3_CONT

MVGHOSTX_RND_3_CONT
          sta GHOSTX_ND
          jmp MVGHOSTX_CONT

MVGHOSTX_RND_2
          ;lda #1
          LDA RANDOM
          cmp #128
          bcc MVGHOSTX_RND_2_1
          lda #1
          jmp MVGHOSTX_RND_2_CONT 
MVGHOSTX_RND_2_1
          lda #0
MVGHOSTX_RND_2_CONT
          sta GHOSTX_ND
          jmp MVGHOSTX_CONT

MVGHOSTX_RND_1
          lda #0
          sta GHOSTX_ND

MVGHOSTX_CONT
          ldy GHOSTX_ND
          ldx TXTSA,Y
          txa
          cmp CHAR_U
          beq MVGHOSTX_UP
          txa
          cmp CHAR_D
          beq MVGHOSTX_DN
          txa
          cmp CHAR_L
          beq MVGHOSTX_LT
          txa
          cmp CHAR_R
          beq MVGHOSTX_RT
MVGHOSTX_UP
          lda GHOSTX_ROW_I
          sec
          sbc #1
          sta GHOSTX_ROW_I
          lda #01
          sta GHOSTX_PREV_MOVE
          jmp DRWGHOST
MVGHOSTX_DN
          lda GHOSTX_ROW_I
          clc
          adc #1
          sta GHOSTX_ROW_I
          lda #04
          sta GHOSTX_PREV_MOVE
          jmp DRWGHOST
MVGHOSTX_LT
          lda GHOSTX_COL_I
          sec
          sbc #1
          sta GHOSTX_COL_I
          lda #02
          sta GHOSTX_PREV_MOVE
          jmp DRWGHOST
MVGHOSTX_RT
          lda GHOSTX_COL_I
          clc
          adc #1
          sta GHOSTX_COL_I
          lda #03
          sta GHOSTX_PREV_MOVE

DRWGHOST
          ldx GHOSTX_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
          lda PLAYER_CHR
          cmp (rowL),Y
          bne DRWGHOST_OK
          jmp GHOST_COLL
DRWGHOST_OK
          lda GHOST_CHR
          sta (rowL),Y
          ldx GHOSTX_ROW_I
          lda ColorRowTableDataL,x
          sta rowL
          lda ColorRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
          lda #7
          sta (rowL),Y

          lda ACTIVE_GHOST
          cmp #01
          beq UPDATE_GHOST_1
          cmp #02
          beq UPDATE_GHOST_2
          cmp #03
          beq UPDATE_GHOST_3
          cmp #04
          beq UPDATE_GHOST_4

UPDATE_GHOST_1
          lda GHOSTX_PREV_MOVE
          sta GHOST1_PREV_MOVE
          stx GHOST1_ROW_I
          sty GHOST1_COL_I
          rts

UPDATE_GHOST_2
          lda GHOSTX_PREV_MOVE
          sta GHOST2_PREV_MOVE
          stx GHOST2_ROW_I
          sty GHOST2_COL_I
          rts

UPDATE_GHOST_3
          lda GHOSTX_PREV_MOVE
          sta GHOST3_PREV_MOVE
          stx GHOST3_ROW_I
          sty GHOST3_COL_I
          rts

UPDATE_GHOST_4
          lda GHOSTX_PREV_MOVE
          sta GHOST4_PREV_MOVE
          stx GHOST4_ROW_I
          sty GHOST4_COL_I
          rts


CLRGHOST
          ;lda #00
          ;sta ACTIVE_GHOST
          ldx GHOSTX_ROW_I
          lda ScreenRowTableDataL,x
          sta rowL
          lda ScreenRowTableDataH,x
          sta rowH
          ldy GHOSTX_COL_I
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
          rts
drwscrn
          incasm "screen.asm"

TXTSA  BYTE $00,$00,$00