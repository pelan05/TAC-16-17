;Trabalho pratico de TAC 16/17 ISEC
;Pedro Lanhoso a21250134
;Ricardo Silva a21260250

.8086
.model small
.stack 2048

dseg   	segment para public 'data'

Erro_Open       db      'Erro ao tentar abrir o ficheiro$'
Erro_Ler_Msg    db      'Erro ao tentar ler do ficheiro$'
Erro_Close      db      'Erro ao tentar fechar o ficheiro$'

msgErrorCreate	db	    "Ocorreu um erro na criacao do ficheiro!$"
msgErrorWrite	  db	    "Ocorreu um erro na escrita para ficheiro!$"
msgErrorClose	  db	    "Ocorreu um erro no fecho do ficheiro!$"

labirinto       db      'lab.txt',0				;labirinto
labirinto2			db			'lab2.txt', 0 		;labirinto bonus
menu1       		db      'menu.txt',0			;menu principal
menu2       		db      'menu2.txt',0			;menu secundario
ganhou_ecra     db      'ganhou.txt',0		;ecra vitoria
lab_temp				db 			'lab_temp.txt',0	;template de labirinto
lab_novo				db			'LAB_NOVO.txt', 0 ;labirinto criado pelo user
perdeu_ecra     db      'perdeu.txt',0		;ecra derrota no bonus
fich            db      ?
HandleFich      dw      0
car_fich        db      ?


;gravar
vetor						db 		3792 dup(0)



Car		        db	    32		; Guarda um caracter do Ecran
Cor		        db	    7			; Guarda os atributos de cor do caracter
POSy		      db	    2			; a linha pode ir de [1 .. 25]
POSx		      db	    6			; POSx pode ir [1..80]
POSya		      db	    2			; Posição anterior de y
POSxa		      db	    6			; Posição anterior de x
POSyn         db	    2			;
POSxn		      db	    6			;
POSyO         db      2			;x e y
POSxO         db      6			;do bonus
handle_Cria   dw      ?
char_cria     db      ?
Cria_POSx     db	    2
Cria_POSy     db 	    6
var1          dw      ?
Carn		      db	    32
corn          db      7


CPOSy         db      5
CPOSx         db      10
CCar          db      32


buff          db      51          ;MAX NUMBER OF CHARACTERS ALLOWED (10).
              db      ?           ;NUMBER OF CHARACTERS ENTERED BY USER.
              db      51 dup(0)   ;CHARACTERS ENTERED BY USER.



ganhouVar     db      0
delaytime     db      5



dseg    	ends

cseg		segment para public 'code'
		assume  cs:cseg, ds:dseg


		goto_xy	macro		POSx,POSy
				mov		ah,02h
				mov		bh,0		        ; numero da pagina
				mov		dl,POSx
				mov		dh,POSy
				int		10h
		endm

















main		proc

mov     ax, dseg
mov     ds, ax
mov			ax, 0b800h
mov			es, ax



inicio:
						xor si, si
						xor di, di
menu:
			call clear_screen

			mov     ah, 3dh		   	; vamos abrir ficheiro para leitura
	    mov     al, 0			    ; tipo de ficheiro
	    lea     dx, menu1			; nome do ficheiro
	    int     21h		        ; abre para leitura

			call ecra

			xor ax, ax
			mov ah, 07h
			int 21h

			cmp al, '1'
			je jogo
			cmp al, '2'
			je top
			cmp al, '3'
			je def
			cmp al, '4'
			je jogob
			cmp al, 27
			je fim
			jmp prox					;fim menu 1





;			          JJJJJJJJJJJ     OOOOOOOOO             GGGGGGGGGGGGG     OOOOOOOOO
;			          J:::::::::J   OO:::::::::OO        GGG::::::::::::G   OO:::::::::OO
;			          J:::::::::J OO:::::::::::::OO    GG:::::::::::::::G OO:::::::::::::OO
;			          JJ:::::::JJO:::::::OOO:::::::O  G:::::GGGGGGGG::::GO:::::::OOO:::::::O
;			            J:::::J  O::::::O   O::::::O G:::::G       GGGGGGO::::::O   O::::::O
;			            J:::::J  O:::::O     O:::::OG:::::G              O:::::O     O:::::O
;			            J:::::J  O:::::O     O:::::OG:::::G              O:::::O     O:::::O
;			            J:::::j  O:::::O     O:::::OG:::::G    GGGGGGGGGGO:::::O     O:::::O
;			            J:::::J  O:::::O     O:::::OG:::::G    G::::::::GO:::::O     O:::::O
;			JJJJJJJ     J:::::J  O:::::O     O:::::OG:::::G    GGGGG::::GO:::::O     O:::::O
;			J:::::J     J:::::J  O:::::O     O:::::OG:::::G        G::::GO:::::O     O:::::O
;			J::::::J   J::::::J  O::::::O   O::::::O G:::::G       G::::GO::::::O   O::::::O
;			J:::::::JJJ:::::::J  O:::::::OOO:::::::O  G:::::GGGGGGGG::::GO:::::::OOO:::::::O
;			 JJ:::::::::::::JJ    OO:::::::::::::OO    GG:::::::::::::::G OO:::::::::::::OO
;			   JJ:::::::::JJ        OO:::::::::OO        GGG::::::GGG:::G   OO:::::::::OO
;			     JJJJJJJJJ            OOOOOOOOO             GGGGGG   GGGG     OOOOOOOOO


jogo:
      call clear_screen

			mov     ah, 3dh		       	; vamos abrir ficheiro para leitura
	    mov     al, 0			        ; tipo de ficheiro
	    lea     dx, labirinto			; nome do ficheiro
	    int     21h			          ; abre para leitura


      call ecra


      goto_xy	POSx,POSy	        ; Vai para nova posição
  		mov 	ah, 08h	            ; Guarda o Caracter que está na posição do Cursor
  		mov		bh, 0		            ; numero da página
  		int		10h
  		mov		Car, al	            ; Guarda o Caracter que está na posição do Cursor
  		mov		Cor, ah	            ; Guarda a cor que está na posição do Cursor


      CICLO:
          goto_xy	POSxa,POSya	  ; Vai para a posição anterior do cursor
  		    mov		ah, 02h
  		    mov		dl, Car	        ; Repoe Caracter guardado
  		    int		21H

  		    goto_xy	POSx,POSy	    ; Vai para nova posição
  		    mov 	ah, 08h
  		    mov		bh, 0		        ; numero da página
  		    int		10h
  		    mov		Car, al	        ; Guarda o Caracter que está na posição do Cursor
  		    mov		Cor, ah	        ; Guarda a cor que está na posição do Cursor

  		    goto_xy	77,1		      ; Mostra o caractr que estava na posição do AVATAR
  		    mov		ah, 02h	        ; IMPRIME caracter da posição no canto
  		    mov		dl, Car
  		    int		21H

  		    goto_xy	POSx,POSy	    ; Vai para posição do cursor
          IMPRIME:
              mov		ah, 02h
  		        mov		dl, 185	    ; Coloca AVATAR
  		        int		21H
  		        goto_xy	POSx,POSy	; Vai para posição do cursor

  		        mov		al, POSx	  ; Guarda a posição do cursor
  		        mov		POSxa, al
  		        mov		al, POSy	  ; Guarda a posição do cursor
  		        mov 	POSya, al

					;LEITURA TECLAS

					LER_SETA:
              call 	LE_TECLA
  		        cmp		ah, 1
  		        je		CIMA
  		        cmp 	al, 27	    ; ESCAPE
  		        je		voltar_jogo
  		        jmp		LER_SETA

          CIMA:
              cmp 	al, 48h
  		        jne		BAIXO
              call   decY
              cmp   ganhouVar, 1
              je    ganhou
              jmp		CICLO

          BAIXO:
              cmp		al, 50h
  		        jne		ESQUERDA
              call    incY
              cmp   ganhouVar, 1
              je    ganhou
              jmp		CICLO

          ESQUERDA:
  		        cmp		al, 4Bh
  		        jne		DIREITA
              call   decX
              cmp   ganhouVar, 1
              je    ganhou
              jmp		CICLO

          DIREITA:
  		        cmp		al, 4Dh
  		        jne		LER_SETA
              call  incX
              cmp   ganhouVar, 1
              je    ganhou
              jmp		CICLO

    ;FIM LEITURA TECLAS
    ;PROCESSOS PARA PAREDES





	    ganhou:
        call clear_screen
        lea  dx, ganhou_ecra
        call ecra
        mov  al, POSyO
        mov  POSy, al
        mov  al, POSxO
        mov  POSx, al
        mov ganhouVar, 0
        goto_xy	POSx,POSy	     ; Vai para nova posição
		          mov  ah, 07h     ; Espera por qualquer tecla.
		          int  21h
		          jmp  voltar_jogo


;FIM PROCESSOS PARA PAREDES


  		voltar_jogo:
              jmp  menu

      jogo_loop:
              jmp jogo



top:
;A IMPLEMENTAR
jmp ganhou


def:
call clear_screen

mov     ah, 3dh			      ; vamos abrir ficheiro para leitura
mov     al, 0			        ; tipo de ficheiro
lea     dx, menu2		    	; nome do ficheiro
int     21h			          ; abre para leitura


call ecra

			xor ax, ax
			mov ah, 01h
			int 21h

			cmp al, '1'
			je Carregar_Base
      cmp al, '2'
			je	Carregar_Ant
			cmp al, '3'
			je	Jogar_Ant
			cmp al, 27
			je voltar		;fim menu 2
			jmp rep_def







Carregar_Base:

call clear_screen

mov     ah, 3dh		    	      ; vamos abrir ficheiro para leitura
mov     al, 0			            ; tipo de ficheiro
lea     dx, lab_temp		      ; nome do ficheiro
int     21h			              ; abre para leitura

call ecra

jmp criar

jmp menu


Jogar_Ant:

call clear_screen

mov     ah, 3dh			      ; vamos abrir ficheiro para leitura
mov     al, 0			        ; tipo de ficheiro
lea     dx, LAB_NOVO			; nome do ficheiro
int     21h			          ; abre para leitura

call ecra
goto_xy	POSx,POSy        	; Vai para nova posição
mov 	ah, 08h	            ; Guarda o Caracter que está na posição do Cursor
mov		bh, 0		            ; numero da página
int		10h
mov		Car, al	            ; Guarda o Caracter que está na posição do Cursor
mov		Cor, ah	            ; Guarda a cor que está na posição do Cursor

jmp CICLO

jmp menu



Carregar_ant:

call clear_screen

mov     ah, 3dh		    	  ; vamos abrir ficheiro para leitura
mov     al, 0			        ; tipo de ficheiro
lea     dx, LAB_NOVO			; nome do ficheiro
int     21h			          ; abre para leitura

call ecra

jmp criar

jmp menu



;CODIGO PARA CRIAR LAB'S NOVOS // GUARDA EM lAB_NOVO.TXT


Criar:
		call		clear_screen

		mov     ah, 3dh			       ; vamos abrir ficheiro para leitura
		mov     al, 0			         ; tipo de ficheiro
		lea     dx, lab_temp		   ; nome do ficheiro
		int     21h			           ; abre para leitura


		call ecra


		mov 	ah,3CH               ; interrup para criar o ficheiro
		mov 	cx,0
		lea 	dx, LAB_NOVO
		int		21h
		mov		handle_Cria, ax

		call ecra

ciclo_cria:	goto_xy	Cria_POSx,Cria_POSy

IMPRIME_Cria:
		mov		ah, 02h
		mov		dl, char_cria
		int		21H
		goto_xy	Cria_POSx,Cria_POSy

		call 	LE_TECLA
		cmp		ah, 1
		je		ESTEND_2
		cmp 	AL, 27		;Esc
		je		sair_cria

UM_Cria:
    CMP 	AL, '1'
		JNE		DOIS_Cria
		mov		char_cria, ' '
		jmp		ciclo_cria

DOIS_Cria:
    CMP   AL, '2'
		JNE 	TRES_Cria
		mov 	char_cria, '&'
		jmp 	ciclo_cria

TRES_Cria:
    CMP 	AL, '3'
		JNE		QUATRO_Cria
		mov		char_cria, 'F'
		jmp		ciclo_cria

QUATRO_Cria:
    cmp AL, '4'
    JNE cont_Cria
    jmp gravar


cont_Cria:		jmp		ciclo_cria

ESTEND_2:								   	;Cima
		cmp 		al,48h
		jne		BAIXO_2
		cmp  	Cria_POSy, 1
		jbe   ciclo_cria
		dec		Cria_POSy
		jmp		ciclo_cria

BAIXO_2:										;Baixo
		cmp		al,50h
		jne		ESQUERDA_2
		cmp   Cria_POSy, 22
		jae   ciclo_cria
		inc 	Cria_POSy
		jmp		ciclo_cria

ESQUERDA_2:								  ;Esquerda
		cmp		al,4Bh
		jne		DIREITA_2
		cmp   Cria_POSx, 1
		jbe   ciclo_cria
		dec		Cria_POSx
		jmp		ciclo_cria

DIREITA_2:									;Direita
		cmp		al,4Dh
		jne		ciclo_cria
		cmp   Cria_POSx,49
		jae   ciclo_cria
		inc		Cria_POSx
		jmp		ciclo_cria

sair_cria:
		mov 	ax,0b800h
		mov 	es,ax
		xor 	si,si


gravar:
		mov 	al, es:[si]
		mov 	ah, es:[si+1]
		mov 	var1,ax
		mov 	ah,40h
		mov 	cx,2
		lea 	dx,var1
		mov		bx,handle_Cria
		int 	21h
		add 	si , 2
		cmp 	si, 3520
		jne 	gravar
		; interrupção para fechar o ficheiro
		mov		ah,3Eh
		mov		bx,handle_Cria
		int		21h

		jmp 	menu

;FIM DO CODIGO PARA CRIAR E GUARDAR LAB'S

jogob:

						call  clear_screen
						lea   dx, labirinto2
						call  ecra
						mov   delaytime, 5
						call  bonus_string
						mov   si, 2
						goto_xy	POSx,POSy	       ; Vai para nova posição
						mov 	ah, 08h	           ; Guarda o Caracter que está na posição do Cursor
						mov		bh, 0		           ; numero da página
						int		10h
						mov		Car, al	           ; Guarda o Caracter que está na posição do Cursor
						mov		Cor, ah	           ; Guarda a cor que está na posição do Cursor



						CICLOb:

						call clear_screen
						lea  dx, labirinto2
						call ecra
								goto_xy	POSxa,POSya	  ; Vai para a posição anterior do cursor
								mov		ah, 02h
								mov		dl, Car	        ; Repoe Caracter guardado
								int		21H

								goto_xy	POSx,POSy	    ; Vai para nova posição
								mov 	ah, 08h
								mov		bh, 0		        ; numero da página
								int		10h
								mov		Car, al 	      ; Guarda o Caracter que está na posição do Cursor
								mov		Cor, ah	        ; Guarda a cor que está na posição do Cursor

								goto_xy	77,1		      ; Mostra o caractr que estava na posição do AVATAR
								mov		ah, 02h	        ; IMPRIME caracter da posição no canto
								mov		dl, Car
								int		21H

								goto_xy	POSx,POSy	    ; Vai para posição do cursor
								IMPRIMEb:
										mov		ah, 02h
										mov		dl, 185	    ; Coloca AVATAR
										int		21H
										goto_xy	POSx,POSy	; Vai para posição do cursor

										mov		al, POSx	  ; Guarda a posição do cursor
										mov		POSxa, al
										mov		al, POSy	  ; Guarda a posição do cursor
										mov 	POSya, al


                    COMP_CHAR:

                        call delay
                        mov al,buff[si]
                        cmp   ganhouVar, 1
                        je    ganhoub
                        cmp al, '$'
                        jne tec0
                        cmp al, '$'
                        je perdeu
                        jmp CICLOb

                        tec0:
                          cmp al, '0'
                          jne tec1
                          call decY
                          inc si
                          jmp CICLOb
                        tec1:
                          cmp al, '1'
                          jne tec2
                          call decY
                          call decY
                          inc si
                          jmp CICLOb
                        tec2:
                          cmp al, '2'
                          jne tec3
                          call decY
                          call decY
                          call decY
                          inc si
                          jmp CICLOb
                        tec3:
                          cmp al, '3'
                          jne tec4
                          call decY
                          call decY
                          call decY
                          call decY
                          inc si
                          jmp CICLOb
                        tec4:
                          cmp al, '4'
                        jne tec5
                        call incY
                        inc si
                        jmp CICLOb
                        tec5:
                        cmp al, '5'
                        jne tec6
                        call incY
                        call incY
                        inc si
                        jmp CICLOb
                        tec6:
                        cmp al, '6'
                        jne tec7
                        call incY
                        call incY
                        call incY
                        inc si
                        jmp CICLOb
                        tec7:
                        cmp al, '7'
                        jne tec8
                        call incY
                        call incY
                        call incY
                        call incY
                        je    perdeu
                        inc si
                        jmp CICLOb
                        tec8:
                        cmp al, '8'
                        jne tec9
                        call incX
                        inc si
                        jmp CICLOb
                        tec9:
                        cmp al, '9'
                        jne tecA
                        call incX
                        call incX

                        inc si
                        jmp CICLOb
                        tecA:
                        cmp al, 'a'
                        jne tecB
                        call incX
                        call incX
                        call incX
                        inc si
                        jmp CICLOb
                        tecB:
                        cmp al, 'b'
                        jne tecC
                        call incX
                        call incX
                        call incX
                        call incX
                        inc si
                        jmp CICLOb
                        tecC:
                        cmp al, 'c'
                        jne tecD
                        call decX
                        inc si
                        jmp CICLOb
                        tecD:
                        cmp al, 'd'
                        jne tecE
                        call decX
                        call decX
                        inc si
                        jmp CICLOb
                        tecE:
                        cmp al, 'e'
                        jne tecF
                        call decX
                        call decX
                        call decX
                        inc si
                        jmp CICLOb
                        tecF:
                        cmp al, 'f'
                        jne COMP_CHAR
                        call decX
                        call decX
                        call decX
                        call decX
                        inc si
                        jmp CICLOb


                            ganhoub:
                                mov ax, 0003H
                                int 10H
                                mov delaytime, 100
                                mov dx, 0000H
                                call clear_screen
                                lea dx, ganhou_ecra
                                call ecra
                                mov  al, POSyO
                                mov  POSy, al
                                mov  al, POSxO
                                mov  POSx, al
                                mov ganhouVar, 0
                                goto_xy	POSx,POSy	; Vai para nova posição

                                mov  ah, 07h      ;Espera por qualquer tecla.
                                int  21h
                                jmp saib


                                perdeu:

                                mov ax, 0003H
                                int 10H
                                mov delaytime, 100
                                mov dx, 0000H
                                call clear_screen
                                lea dx, perdeu_ecra
                                call ecra
                                mov  al, POSyO
                                mov  POSy, al
                                mov  al, POSxO
                                mov  POSx, al
                                goto_xy	POSx,POSy	  ; Vai para nova posição

                                mov  ah, 07h        ;Espera por qualquer tecla.
                                int  21h
                                jmp saib

                                saib:
                                jmp menu

voltar:
		jmp  menu

rep_def:
		jmp def

sair:
		mov  ax, 4c00h
		int  21h

prox:
		jmp menu

fim:
		mov     ah,4ch
		int     21h


main		endp


ecra proc

    mov     ah,3dh		      	; vamos abrir ficheiro para leitura
    mov     al,0			        ; tipo de ficheiro
    int     21h			          ; abre para leitura
    jc      erro_abrir		    ; pode aconter erro a abrir o ficheiro
    mov     HandleFich,ax		  ; ax devolve o Handle para o ficheiro
    jmp     ler_ciclo		      ; depois de abero vamos ler o ficheiro

    erro_abrir:
    mov     ah,09h
    lea     dx,Erro_Open
    int     21h
    jmp     sai

    ler_ciclo:
    mov     ah,3fh			     ; indica que vai ser lido um ficheiro
    mov     bx,HandleFich		 ; bx deve conter o Handle do ficheiro previamente aberto
    mov     cx,1			       ; numero de bytes a ler
    lea     dx,car_fich		   ; vai ler para o local de memoria apontado por dx (car_fich)
    int     21h				       ; faz efectivamente a leitura
    jc	    erro_ler		     ; se carry é porque aconteceu um erro
    cmp	    ax,0			       ;EOF?	verifica se já estamos no fim do ficheiro
    je	    fecha_ficheiro	 ; se EOF fecha o ficheiro
    mov     ah,02h			     ; coloca o caracter no ecran
    mov	    dl,car_fich		   ; este é o caracter a enviar para o ecran
    int	    21h				       ; imprime no ecran
    jmp	    ler_ciclo		     ; continua a ler o ficheiro

    erro_ler:
    mov     ah,09h
    lea     dx,Erro_Ler_Msg
    int     21h

    fecha_ficheiro:					 ; vamos fechar o ficheiro
    mov     ah,3eh
    mov     bx,HandleFich
    int     21h
    jnc     sai

            mov     ah,09h  ; o ficheiro pode não fechar correctamente
            lea     dx,Erro_Close
            Int     21h
    sai:
            ret


ecra  endp







clear_screen proc
mov ah, 0
mov al, 3
int 10h
ret
clear_screen endp



LE_TECLA	PROC

mov		ah,08h
int		21h
mov		ah,0
cmp		al,0
jne		SAI_TECLA
mov		ah, 08h
int		21h
mov		ah,1
SAI_TECLA:	RET
LE_TECLA	endp







cria_maze proc

		;Obter a posição
		dec		CPOSy		; linha = linha -1
		dec		CPOSx		; POSx = POSx -1

CICLO:	goto_xy	CPOSx,CPOSy
IMPRIME:	mov		ah, 02h
	mov		dl, CCar
	int		21H
	goto_xy	CPOSx,CPOSy

	call 		LE_TECLA
	cmp		ah, 1
	je		ESTEND
	CMP 		AL, 27	          ; ESCAPE
	JE		FIM

ZERO:		CMP 		AL, 48		  ; Tecla 0
	JNE		UM
	mov		CCar, 32		        ;Espaço
	jmp		CICLO

UM:		CMP 		AL, 49		    ; Tecla 1
	JNE		DOIS
	mov		CCar, '&'		        ;Caracter CHEIO
	jmp		CICLO

DOIS:		CMP 		AL, 50	  	; Tecla 2
	JNE		NOVE
	mov		CCar, 'F'		        ;CINZA 177
	jmp		CICLO

NOVE:		jmp		CICLO

ESTEND:	cmp 		al, 48h
	jne		BAIXO
	dec		CPOSy		            ;cima
	jmp		CICLO

BAIXO:	cmp		al,50h
	jne		ESQUERDA
	inc 		CPOSy		          ;Baixo
	jmp		CICLO

ESQUERDA:
	cmp		al, 4Bh
	jne		DIREITA
	dec		CPOSx		           ;Esquerda
	jmp		CICLO

DIREITA:
	cmp		al, 4Dh
	jne		CICLO
	inc		CPOSx		           ;Direita
	jmp		CICLO

fim:
	ret
cria_maze endp

delay proc

    ;this procedure uses 1A interrupt, more info can be found on
    ;http://www.computing.dcu.ie/~ray/teaching/CA296/notes/8086_bios_and_dos_interrupts.html
    mov ah, 00
    int 1Ah
    mov bx, dx

jmp_delay:
    int 1Ah
    sub dx, bx
    ;there are about 18 ticks in a second, 10 ticks are about enough
    cmp dl, delaytime
    jl jmp_delay
    ret

delay endp


decY proc
    mov al, POSy
    mov POSyn, al
    dec POSyn
    goto_xy	POSx,POSyn
    mov 	ah, 08h
    mov		bh,0		          ; numero da página
    int		10h
    mov		Carn, al	        ; Guarda o Caracter que está na posição do Curso
    cmp   Carn, '&'
    je return
    cmp   Carn, 'F'
    je    ganhouProc

    mov   al, POSyn
    mov   POSy,al

    jmp return
    ganhouProc:
    mov al,1
    mov ganhouVar, al
    ret

    return:
    mov   al, POSya
    mov   POSyn,al
    ret
decY endp

incY proc
    mov al, POSy
    mov POSyn, al
    inc POSyn
    goto_xy	POSx,POSyn
    mov 	ah, 08h
    mov		bh,0		         ; numero da página
    int		10h
    mov		Carn, al	       ; Guarda o Caracter que está na posição do Cursor
    cmp   Carn, '&'
    je return
    cmp   Carn, 'F'
    je    ganhouProc

    mov   al, POSyn
    mov   POSy,al

    jmp return
    ganhouProc:
    mov al,1
    mov ganhouVar, al
    ret

    return:
    mov   al, POSya
    mov   POSyn,al
    ret
incY endp


incX proc
    mov al, POSx
    mov POSxn, al
    inc POSxn
    goto_xy	POSxn,POSy
    mov 	ah, 08h
    mov		bh,0		         ; numero da página
    int		10h
    mov		Carn, al	       ; Guarda o Caracter que está na posição do Cursor
    cmp   Carn, '&'
    je    return
    cmp   Carn, 'F'
    je    ganhouProc

    mov   al, POSxn
    mov   POSx,al

    jmp return
    ganhouProc:
    mov al,1
    mov ganhouVar, al
    ret

    return:
    mov   al, POSxa
    mov   POSxn,al
    ret
incX endp

decX proc
    mov al, POSx
    mov POSxn, al
    dec POSxn
    goto_xy	POSxn,POSy
    mov 	ah, 08h
    mov		bh,0		           ; numero da página
    int		10h
    mov		Carn, al	         ; Guarda o Caracter que está na posição do Cursor
    cmp   Carn, '&'
    je return
    cmp   Carn, 'F'
    je    ganhouProc

    mov   al, POSxn
    mov   POSx,al

    jmp return

    ganhouProc:
    mov al,1
    mov ganhouVar, al
    ret

    return:
    mov   al, POSxa
    mov   POSxn,al
    ret
decX endp



bonus_string proc
;CAPTURE STRING FROM KEYBOARD.
            mov dl,0
            xor si,si
            xor di,di

            mov ah, 0Ah ;SERVICE TO CAPTURE STRING FROM KEYBOARD.
            mov dx, offset buff
            int 21h
            call clear_screen
;CHANGE CHR(13) BY '$'.
            mov si, offset buff + 1 ;NUMBER OF CHARACTERS ENTERED.
            mov cl, [ si ] ;MOVE LENGTH TO CL.
            mov ch, 0      ;CLEAR CH TO USE CX.
            inc cx ;TO REACH CHR(13).
            add si, cx ;NOW SI POINTS TO CHR(13).
            mov al, '$'
            mov [ si ], al ;REPLACE CHR(13) BY '$'.
            ret
bonus_string endp



cseg    	ends
end     	Main
