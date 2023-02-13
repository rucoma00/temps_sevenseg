;*******************************************************************************
;   	Ejemplo de realización de un temporizador digital que muestra décimas, 
;	segundos y minutos que restan para llegar a 0:00.0. Consta de dos 
;	pulsadores: uno de STOP (en RB0) y otro de START (en RA4) 
;	Se emplean tres displays de 7 segmentos de ánodo común y punto decimal,
;	gobernados por el puerto D, y de un diodo LED conectado a RB3.
;
;	Con el crono en su estado inicial (ESTADO 0, parado y configurable),
;	una pulsación en el botón STOP modificará el tiempo. Una pulsación en el
;	botón START pone el sistema en marcha (ESTADO 1, cuenta).
;	En ese estado, si se pulsa STOP, el temporizador se para (ESTADO 2,
;	parado y no configurable). Si se vuelve a pulsar STOP, el temporizador 
;	vuelve a su estado inicial, pero si se pulsa START, la temporización
;	descendente continua. El LED esta encendido en el ESTADO 0, parpadeando
;	en el ESTADO 1 y apagado en el ESTADO 2.
;
;	Se utiliza una técnica de barrido secuencial de los displays, los 
;	segmentos se controlan desde el puerto D 
;	(RD0->a,RD1->b,...,RD6->g,RD7->dp) 
;	y los ánodos comunes de los displays con 3 líneas del puerto A 
;	(RA3->display derecho, RA2->display central, RA1->display izquierdo)
;	 
;	Si se llega al final de la temporización, se muestra el tiempo inicial. 
;	El temporizador mostrara inicialmente 15 segundos. Mediante el pulsador
;	STOP podremos ir sumando 15 segundos hasta alcanzar un máximo de 
;	5 minutos (ESTADO 0, parado y tiempo configurable). Cuando se alcancen, 
;	una nueva pulsación en STOP hara que se vuelvan a mostrar 15 segundos.
;	 
;	Frecuencia del oscilador 4MHz
;
;       Autor: Rubén Concejo Martínez
;
;*******************************************************************************

        list p=16f877A		;Para microcontrolador PIC16f877A
        include P16f877A.inc	;Incluimos fichero con etiquetas de registros y bits
	 
	;Cargamos bits de la palabra de configuración
	 __CONFIG    _XT_OSC & _WDT_OFF & _LVP_OFF	

	;Declaración de posiciones para variables auxiliares, a partir de 0x20
	CBLOCK	0x20
	;En los 4 bits más bajos de las siguientes variables se almacenan:
	DECIMAS		;Almacenamiento de décimas de segundo a representar
	UN_SEGUNDOS	;Almacenamiento de unidades de segundo a representar
	DC_SEGUNDOS	;Almacenamiento de decenas de segundo a representar
	MINUTOS		;Almacenamiento de minutos a representar
	
	W_tmp     	;Salvaguarda de W en interrupción
	STATUS_tmp 	;Salvaguarda de STATUS en interrupción
	PCLATH_tmp	;Para salvar PCLATH en la interrupción

	ESTADO		;Aquí guardaremos el estado en el que se puede encontrar el crono:	
			;0 -> Reseteado	    1 -> Contando	2-> Parado
	
	;Se guardan los valores iniciales para volver a ellos al final de la cuenta
	UN_SEG_AUX	;Variable auxiliar que guarda las unidades de segundo iniciales 
	DC_SEG_AUX	;Variable auxiliar que guarda las decenas de segundo iniciales
	MINUTOS_AUX	;Variable auxiliar que guarda los minutos iniciales
	
	DESBORDA_TMR2	;Contador de desbordamientos de TMR2
	
	ENDC		;Fin de la declaración de variables
	
	org 0            	;Vector de RESET
        goto INICIO		;primera instrucción que se ejecuta

        org 4	    		;Posición para interrupciones generadas
        goto TEMPO  		;por TMR1, iremos a la etiqueta indicada
	
; El programa principal saltará a esta posición para configurar inicialmente los módulos internos       

INICIO  movlw 	0xFF	    ;Preparamos la salida del PORTA (control de displays)
	movwf 	PORTA	    ;Desactivamos displays (cuando el PORTA sea de salida)
	bsf	PORTB,3	    ;LED encendido inicialmente (ESTADO 0)
	bsf 	STATUS,RP0  ;Pasamos al banco 1 de datos 
        clrf 	TRISD       ;Puerto D se programa como salida (control de los segmentos)
	movlw 	b'11110001' ;Puerto A con 3 líneas de salida para selección de displays
        movwf	TRISA       ;Salidas del Puerto A: RA1, RA2 y RA3
	movlw	b'00000111' ;Definimos el PORTA
	movwf	ADCON1	    ;con todas sus señales como digitales
	bcf	TRISB,3	    ;RB3 como salida
	
;El resto de puertos quedan como puertos de entrada (por defecto)
	
;Configuración de los contadores	
	movlw 	b'00000110' ;TMR0 como temporizador y prescaler de 128
        movwf	OPTION_REG  ;asignado a TMR0 para temporizar 5ms en los barridos
			    ;activación de INTF por flanco de bajada para detectar pulsaciones en RB0
	bsf	PIE1,TMR1IE ;Activamos máscara de interrupciones de TMR1
	bsf	PIE1,TMR2IE ;y la del TMR2
	movlw	d'194'	    ;Cargamos 194 en W para pasárselo al registro PR2
	movwf	PR2	    ;y así asegurar 50ms de tiempo de desbordamiento para TMR2
			    ;se cuentan 10 desbordamientos para temporizar medio segundo
       	bcf 	STATUS,RP0  ;Volvemos al banco 0 de RAM de datos
	
	movlw	b'00010000' ;Configuramos TMR1 en modo temporizador, prescaler de 2
	movwf	T1CON	    ;e inicialmente parado
	
	movlw	b'01111011' ;Configuramos TMR2 con prescaler y postscaler de 16
	movwf	T2CON	    ;e inicialmente apagado
	
	movlw	0x3C	    ;Precargamos TMR1H
	movwf	TMR1H	    ;puesto que está parado TMR1
	movlw	0xB0	    ;y cargamos TMR1L
	movwf	TMR1L	    ;para que desborde al cabo de 0,1 s cuando se ponga en marcha
	
;Inicialización de variables
	clrf 	DECIMAS	    ;Puesta a cero de contador de décimas de segundo
	movlw	d'5'	    ;Unidades de segundo inicialmente a 5
	movwf 	UN_SEGUNDOS ;Se carga 5 en las unidades de segundo
	movwf	UN_SEG_AUX  ;y en su variable auxiliar asociada
	movlw	d'1'	    ;Decenas de segundo inicialmente a 1
	movwf 	DC_SEGUNDOS ;Se carga 1 en las decenas de segundo
	movwf	DC_SEG_AUX  ;y en su variable auxiliar asociada
	clrf 	MINUTOS     ;Puesta a cero del contador de minutos
	clrf	MINUTOS_AUX ;Puesta a cero de la variable auxiliar asociada a los minutos
	clrf	DESBORDA_TMR2	;Comienza a 0
	clrf	ESTADO	    ;Estado inicial RESETEADO
	
;Interrupciones desactivadas tras el RESET, programamos interrupciones del TMR1 

	movlw	b'11000000'	;Habilitamos las interrupciones con
	movwf	INTCON		;GIE=1 y PEIE=1, (TMR1IE ya se activó arriba)

;INICIO DEL BUCLE
;Empieza el bucle continuo de ejecución con el barrido 
;se recogen los contenidos de las posiciones DECIMAS, UN_SEGUNDOS, DC_SEGUNDOS 
;y MINUTOS y los muestra en los displays derecho, central e izquierdo 
;respectivamente.

BUCLE	
	call	BARRIDO		;Realizamos barrido displays
;Ahora ramificamos el código en función del estado en el que estamos
	movf	ESTADO,W	;para ello traspasamos ESTADO a W
	addwf	PCL,F		;y lo sumamos al PC, para ir a 3 etiquetas distintas
	goto	ESTADO_0
	goto	ESTADO_1
	goto	ESTADO_2
	
;En función de ESTADO se definen las salidas y también los posibles cambios de estado	
ESTADO_0	;Temporizador parado y configurable con el pulsador STOP
	bsf	PORTB,3		;ESTADO = 0 => LED ON 
	;Se miran los pulsadores
	btfsc	PORTA,4		;Se mira el pulsador START
	goto	MIRAR_RB0_0	;Si no esta pulsado (RA4=1) se mira STOP
	movlw	0x01		;Si esta pulsado (RA4=0) se cambia ESTADO a 0x01
	movwf	ESTADO		;se cambia ESTADO a 0x01
	clrf	DESBORDA_TMR2	;Se pone a 0 el contador de desbordamientos
	bsf	T1CON,TMR1ON	;Se enciende el TMR1
	bsf	T2CON,TMR2ON	;Se enciende TMR2
	goto	BUCLE		;y se va al final de la rama
	
MIRAR_RB0_0			;Se mira STOP mediante el flag INTF, que estará  
	btfss	INTCON,INTF	;a 1 si hay una pulsación
	goto	BUCLE		;Si no hay pulsación se va al final de la rama (INTF=0)
	call	SUMA		;Si la hay, se incrementa el tiempo 
	call	GUARDAR_AUX	;y se guardan los nuevos valores en las 
				;variables auxiliares del tiempo
	bcf	INTCON,INTF	;Se pone el falg a cero por software
	goto	BUCLE		;Se vuelve al comienzo del BUCLE (final de la rama)
		
ESTADO_1	;Temporizador en marcha
	call	PARPADEO	;La función lo hace parpadear
	;Se mira el pulsador STOP
	btfss	INTCON,INTF	;Se mira si hay una pulsación en STOP (INTF=1)
	goto	BUCLE		;Si no la hay se va al final de la rama (INTF=0)
	movlw	0X02		;Si la hay: 
	movwf	ESTADO		;se cambia ESTADO a 0x02 (INTF=1),
	bcf	T1CON,TMR1ON	;se para el TMR1
	bcf	T2CON,TMR2ON	;se para el TMR2
	clrf	DESBORDA_TMR2	;Se resetea el contador de desbordamientos del TMR2
	bcf	INTCON,INTF	;y se pone el flag a cero por software
	goto	BUCLE		;Se vuelve al comienzo del bucle (final de la rama)
	
ESTADO_2	;Temporizador parado y no configurable
	bcf	PORTB,3		;ESTADO = 2 => LED OFF
	;Ahora se miran los pulsadores
	btfsc	PORTA,4		;Se mira el pulsador START
	goto	MIRAR_RB0_2	;Si no esta pulsado (RA4=1) se mira STOP
	movlw	0x01		;Si esta pulsado (RA4=0) se vuelve al ESTADO 1 
	movwf	ESTADO		;Se cambia ESTADO a 0x01
	bsf	T1CON,TMR1ON	;Se enciende el temporizador (paso a ESTADO 1)
	bsf	T2CON,TMR2ON	;y también el TMR2
	goto	BUCLE		;y se va al final de la rama
	
MIRAR_RB0_2			;Se mira STOP mediante el flag INTF, que estará  
	btfss	INTCON,INTF	;a 1 si hay una pulsación
	goto	BUCLE		;Si no hay pulsación se va al final de la rama (INTF=0)
	movlw	0x00		;Si la hay (INTF=1), se va al ESTADO 0
	movwf	ESTADO		;Se cambia ESTADO a 0x00
	bcf	T1CON,TMR1ON	;Se para el TMR1
	bcf	T2CON,TMR2ON	;y se para TMR2
	movlw	0x3C		;Precargamos TMR1H
	movwf	TMR1H		;puesto que está parado TMR1
	movlw	0xB0		;y cargamos TMR1L
	movwf	TMR1L		;para que desborde tras 0.1s cuando se ponga en marcha
	clrf	DECIMAS		;Se resetean las DECIMAS
	call	DEVOLVER_AUX	;y se cargan las variables auxiliares en las variables
				;de tiempo para volver al tiempo inicial
	bcf	INTCON,INTF	;Se pone el flag a cero por software
	goto	BUCLE		;y se vuelve al comienzo del bucle (final de la rama)

;FIN DEL BUCLE 

;******************************************************************************
;*	Subprograma BARRIDO para mostrar los valores en los displays en       * 
;*	función de si los minutos son cero o no                               *                       
;******************************************************************************
BARRIDO	
	;Comprobación de si los minutos son cero o no
	movlw	0x00		;Se mira si los MINUTOS son 0
	xorwf	MINUTOS,W	;Si son 0, Z=1. Si son distintos de, Z=0
	btfss	STATUS,Z	;Se mira el bit Z 
	goto	CON_MINUTOS	;Si son distintos de 0, si se muestran (sin DECIMAS)
	goto	SIN_MINUTOS	;Si son 0, no se muestran los MINUTOS pero si las DECIMAS
	
CON_MINUTOS
	;DISPLAY DERECHO. Se mustran las UNIDADES
	movlw 	b'11110111'     ;Activamos el display de la derecha con
    	movwf 	PORTA          	;el bit RA3 a 0 y el resto a 1
	movf	UN_SEGUNDOS,W	;W=UNIDADES
	call	TABLALED	;Subprograma que carga en W el valor binario que 
				;se ha de pasar al display
	movwf	PORTD		;Se saca por el PORTD los LEDs del display a iluminar
	call	ESPERA		;Espera de 5 ms para el barrido secuencial
	movlw	0xFF		;Puerto D todos los bits a 1
	movwf	PORTD		;Para apagarlos y hacer un tiempo muerto antes del cambio
	
	;DISPLAY CENTRAL. Se muestran las DECENAS 
	movlw 	b'11111011'     ;Activamos el display de central con
    	movwf 	PORTA          	;el bit RA2 a 0 y el resto a 1
	movf	DC_SEGUNDOS,W	;W=DECENAS
	call	TABLALED	;Subprograma que carga en W el valor binario que 
				;se ha de pasar al display
	movwf	PORTD		;Se saca por el PORTD los LEDs del display a iluminar
	call	ESPERA		;Espera de 5 ms para el barrido secuencial
	movlw	0xFF		;Puerto D todos los bits a 1
	movwf	PORTD		;para apagar todos los segmentos

	;DISPLAY IZQUIERDO. Se muestran lOS MINUTOS con punto decimal 
	movlw 	b'11111101'     ;Activamos el display de la izquierda con
    	movwf 	PORTA          	;el bit RA1 a 0 y el resto a 1
	movf	MINUTOS,W	;W=MINUTOS
	call	TABLALED	;Subprograma que carga en W el valor binario que 
				;se ha de pasar al display
	movwf	PORTD		;Se saca por el PORTD los LEDs del display a iluminar
	bcf	PORTD,7		;Se activa el punto decimal del display izquierdo
	call	ESPERA		;Espera de 5 ms para el barrido secuencial
	movlw	0xFF		;Puerto D todos los bits a 1
	movwf	PORTD		;para apagar todos los segmentos
	return			;Retorno de la función
	
SIN_MINUTOS
	;DISPLAY DERECHO. Se mustran las DECIMAS
	movlw 	b'11110111'     ;Activamos el display de la derecha con
    	movwf 	PORTA          	;el bit RA3 a 0 y el resto a 1
	movf	DECIMAS,W	;W=DECIMAS
	call	TABLALED	;Subprograma que carga en W el valor binario que 
				;se ha de pasar al display
	movwf	PORTD		;Se saca por el PORTD los LEDs del display a iluminar
	call	ESPERA		;Espera de 5 ms para el barrido secuencial
	movlw	0xFF		;Puerto D todos los bits a 1
	movwf	PORTD		;Para apagarlos y hacer un tiempo muerto antes del cambio
	
	;DISPLAY CENTRAL. Se muestran las UNIDADES con punto decimal
	movlw 	b'11111011'     ;Activamos el display de central con
    	movwf 	PORTA          	;el bit RA2 a 0 y el resto a 1
	movf	UN_SEGUNDOS,W	;W=UN_SEGUNDOS
	call	TABLALED	;Subprograma que carga en W el valor binario que 
				;se ha de pasar al display
	movwf	PORTD		;Se saca por el PORTD los LEDs del display a iluminar
	bcf	PORTD,7		;Se activa el punto decimal del display central
	call	ESPERA		;Espera de 5 ms para el barrido secuencial
	movlw	0xFF		;Puerto D todos los bits a 1
	movwf	PORTD		;para apagar todos los segmentos
	
	;DISPLAY IZQUIERDO. Se muestran las DECENAS 
	movlw 	b'11111101'     ;Activamos el display de la izquierda con
    	movwf 	PORTA          	;el bit RA1 a 0 y el resto a 1
	movf	DC_SEGUNDOS,W	;W=DECENAS
	call	TABLALED	;Subprograma que carga en W el valor binario que 
				;se ha de pasar al display
	movwf	PORTD		;Se saca por el PORTD los LEDs del display a iluminar
	call	ESPERA		;Espera de 5 ms para el barrido secuencial
	movlw	0xFF		;Puerto D todos los bits a 1
	movwf	PORTD		;para apagar todos los segmentos
	return			;Retorno de la función
	
;Final del subprograma del barrido secuencial

;******************************************************************************
;Subprograma para el control de los diodos led                                *
;Recibe en los cuatro ultimos bits de W el digito a representar: del 0 al 9   *
;******************************************************************************
;OJO CON LA POSICIÓN DE LA TABLA
;¿HAY QUE CARGAR EL PCLATH ANTES? -> NO, en la tabla, todas las posiciones tienen PCLATH=0x00

TABLALED
	addwf 	PCL,F       	;Suma del PC con el dígito a representar (en W)
        retlw 	0xC0          	;Para el cero b'11000000' = 0xC0
        retlw 	0xF9          	;el uno
        retlw 	0xA4          	;el dos
        retlw 	0xB0          	;el tres
        retlw 	0x99          	;el cuatro
        retlw 	0x92          	;el cinco
        retlw 	0x82          	;el seis
        retlw 	0xF8          	;el siete
        retlw 	0x80          	;el ocho
        retlw 	0x90          	;el nueve
	
 ;Final de la tabla de los segmentos a iluminar	
 
;******************************************************************************
;*	Subprograma de Espera de 5ms (aprox.),                                *
;*	se emplea el TMR0 para la temporización,                              *
;*	lo precargamos y esperamos a que desborde                             *
;******************************************************************************

ESPERA	movlw	d'217'		;precargamos el valor de TMR0
	movwf	TMR0		;para que desborde tras 5ms

	bcf	INTCON,T0IF	;Se pone a cero el flag de TMR0
NO_REBOSO		
	btfss	INTCON,T0IF	;Comprobamos si el flag se puso a 1
	goto	NO_REBOSO	;si no se puso a 1, seguimos esperando
	return			;Si ya se puso a 1, retornamos (pasaron ya 5 ms)

;Final del subprograma de espera

;******************************************************************************
;Subprograma de suma con ajuste a digitos decimales de segundos y minutos     *
;Se suman 15segundos hasta llegar a 5 minutos                                 *
;******************************************************************************

SUMA    movlw	0x05		;Se comprueba silos minutos han llegado a 5
	xorwf	MINUTOS,W	;Si han llegado, Z=1. Si no, Z=0    
	btfss	STATUS,Z	;Miro Z
	goto	SI_SUMA		;Si no han llegado, sumo 1 a las DECENAS y 5 a las UNIDADES
	clrf	MINUTOS		;Si han llegado, modifico las variables para que
	movlw	0x01		;los MINUTOS valgan cero y las DECENAS de segundo 
	movwf	DC_SEGUNDOS	;valgan 1. Se carga 1 en las DECENAS de segundo
	movlw	0x05		;Las UNIDADES de segundo han de valer 5
	movwf	UN_SEGUNDOS	;Se carga 5 en las UNIDADES de segundo
	return			;Así se vuelven a mostrar 15s y se retorna
	
SI_SUMA		;Si todavia no se han alcanzado 5 minutos se realiza la suma
	movlw	0x05		;Se suma 5
	addwf	UN_SEGUNDOS	;a las UNIDADES de segundo
	movlw	0x01		;y se suma 1
	addwf	DC_SEGUNDOS,F	;a las DECENAS
	movlw	0x0A		;Se comprueba si las UNIDADES de segundo han
	xorwf	UN_SEGUNDOS,W	;llegado a A (si llegan a 10 me llevo una)
	btfss	STATUS,Z	;Si han llegado, Z=1. Si no, Z=0
	return			;Si no han llegado, retorno, no me llevo nada
	clrf	UN_SEGUNDOS	;Si han llegado, las unidades cambian a 0
	movlw	0x01		;y sumo 1 más a las decenas
	addwf	DC_SEGUNDOS,F	;las DECENAS solo llegan a 6 cuando las UNIDADES
	movlw	0x06		;llegan a A. Compruebo si DECENAS ha llegado a 6
	xorwf	DC_SEGUNDOS,W	;Si han llegado, Z=1. Si no, Z=0 
	btfss	STATUS,Z	;Miro Z
	return			;Si no son 6 retorno (X:30.0 o X:00.0)
	movlw	0x01		;Si son 6 se suma 1 
	addwf	MINUTOS,F	;a los minutos
	clrf	DC_SEGUNDOS	;y se ponen a 0 las DECENAS
	return			;Se retorna. UNIDADES y DECIMAS ya eran 0
	
;Final del subprograma de suma

;******************************************************************************
;*  Subprograma para guardar las variables auxiliares que sirven para poder   *
;*  volver al valor de tiempo inicial al finalizar la temporización           *
;******************************************************************************
	
GUARDAR_AUX
	movf	MINUTOS,W	;Se mueven los minutos a W
	movwf	MINUTOS_AUX	;y luego a su variable auxiliar
	movf	DC_SEGUNDOS,W	;Se mueven las decenas de segundo a W
	movwf	DC_SEG_AUX	;y luego a su variable auxiliar
	movf	UN_SEGUNDOS,W	;Se mueven las unidades de segundo a W
	movwf	UN_SEG_AUX	;y luego a su variable auxiliar
	return			;Retorno. No hace falta guardar las DECIMAS
	
;Final del subprograma GUARDAR_AUX
	
;******************************************************************************
;*  Subprograma para devolver las variables auxiliares que sirven para poder  *
;*  volver al valor de tiempo inicial al finalizar la temporización.          *
;*  Es la inversa de GUARDAR_AUX					      *
;******************************************************************************	
DEVOLVER_AUX
	movf	UN_SEG_AUX,W	;Se cargan las UNIDADES auxiliares	
	movwf	UN_SEGUNDOS	;en su variable correspondiente
	movf	DC_SEG_AUX,W	;Se cargan las DECENAS auxiliares
	movwf	DC_SEGUNDOS	;en su variable correspondiente
	movf	MINUTOS_AUX,W	;Se cargan los MINUTOS auxiliares
	movwf	MINUTOS		;en su variable correspondiente
	return			;Retorno. No hace falta recuperar DECIMAS
	
;Final del subprograma DEVOLVER_AUX
		
;******************************************************************************
;*	Subprograma para el PARPADEO del LED				      *
;*	Comprueba si TMR2 ha desbordado 10 veces (0.5s) y complementa	      *
;*	el LED								      *
;******************************************************************************

PARPADEO	;Se llama a esta función cada vez que entro al ESTADO 1
	movlw	d'10'		;Se compara si el TMR2 ha desbordado 10 veces
	xorwf	DESBORDA_TMR2,W	;Ha desbordado 10 veces => Z=1
	btfss	STATUS,Z	;Todavía no ha desbordado 10 veces => Z=0
	return			;Retorno si todavia no ha desbordado 10 veces
	btfss	PORTB,3		;Miramos si el LED estaba ON (1) o OFF (0)
	goto	ENCENDER	;Si estaba apagado lo enciendo
	bcf	PORTB,3		;Si estaba encendido lo apago
R_DESB	clrf	DESBORDA_TMR2	;Se resetea el contador de desbordamientos
	return			;y se vuelve a la rama del ESTADO 1

ENCENDER	;Si estaba apagado, se enciende
	bsf	PORTB,3		;Se enciende el LED
	goto	R_DESB		;y se vuelve al reseteo del contador de desbordamientos
;Final del subprograma PARPADEO
	
;******************************************************************************
;*	Subprograma RESTA para disminuir el tiempo en una décima.	      *
;*	Cuando el tiempo llega a 0:00.0 se vuelve a poner al                  *			
;*	valor inicial							      *
;******************************************************************************

RESTA
	decfsz	DECIMAS,F	;Se decrementan las DECIMAS y se mira si llegaron a 0
	goto	SI_RESTA	;Si no llegan a 0, se continua restando el resto de variables
	
	;Si las DECIMAS son 0, compruebo si estoy en 0:00.0
	movlw	0x00		;Compruebo si las UNIDADES también son cero
	xorwf	UN_SEGUNDOS,W	;Si lo son, Z=1. Si no lo son, Z=0
	btfss	STATUS,Z	;Miro Z
	return			;Si las UNIDADES no son 0, retorno
	
	;Si las UNIDADES son 0, sigo comprobando
	movlw	0x00		;Compruebo si las DECENAS también son cero
	xorwf	DC_SEGUNDOS,W	;Si lo son, Z=1. Si no lo son, Z=0
	btfss	STATUS,Z	;Miro Z
	return			;Si las DECENAS no son 0, retorno
	
	;Si las DECENAS son 0, sigo comprobando
	movlw	0x00		;Compruebo si los MINUTOS también son cero
	xorwf	MINUTOS,W	;Si lo son, Z=1. Si no lo son, Z=0
	btfss	STATUS,Z	;Miro Z
	return			;Si los MINUTOS nos son 0, retorno
	
	;Si los minutos son 0, llegué a 0:00.0 por lo tanto vuelvo al tiempo 
	;inicial guardado en las variables AUX
	call	DEVOLVER_AUX	;Se devuelven las variables de tiempo a su valor inicial
	;Se devuelve el TMR1 a su configuración inicial
	bcf	T1CON,TMR1ON	;Se apaga el temporizador
	movlw	0x3C		;Precargamos TMR1H
	movwf	TMR1H		;a su valor inicial
	movlw	0xB0		;y cargamos TMR1L
	movwf	TMR1L		;para que desborde tras 0.1s cuando se ponga en marcha
	clrf	ESTADO		;Volvemos al ESTADO 0
	return			;Retorno al PTI 
	
SI_RESTA 	;Se comprueba si las DECIMAS son FF (valor siguiente a 0) y se 
		;continua restando y comprobando de la misma forma
	movlw	0xFF		;Se compara DECIMAS con 0xFF
	xorwf	DECIMAS,W	;Si son 0xFF, Z=1. Si no son 0xFF, Z=0
	btfss	STATUS,Z	;miro Z
	return			;Si no son 0xFF, retorno
	movlw	0x09		;Si son 0xFF,  
	movwf	DECIMAS		;cargo las DECIMAS a 9
	
	;Resta y comprobación de las unidades
	decf	UN_SEGUNDOS,F	;Resto uno a las UNIDADES
	movlw	0xFF		;Se comparan las UNIDADES con 0xFF
	xorwf	UN_SEGUNDOS,W	;Si son 0xFF, Z=1. Si no son 0xFF, Z=0
	btfss	STATUS,Z	;miro Z
	return			;Si no son 0xFF, retorno
	movlw	0X09		;Si son 0xFF,
	movwf	UN_SEGUNDOS	;cargo las DECIMAS a 9
	
	;Resta y comprobación de las DECENAS
	decf	DC_SEGUNDOS,F	;Resto uno a las DECENAS
	movlw	0xFF		;Se comparan las DECENAS con 0xFF
	xorwf	DC_SEGUNDOS,W	;Si son 0xFF, Z=1. Si no son 0xFF, Z=0
	btfss	STATUS,Z	;miro Z
	return			;Si no son 0xFF, retorno
	movlw	0X05		;Si son 0xFF,
	movwf	DC_SEGUNDOS	;cargo las DECENAS a 5
	
	;Resta de MINUTOS y retorno al PTI
	decf	MINUTOS,F	;Resto 1 a los MINUTOS
	movlw	0xFF		;Se comparan los MINUTOS con 0xFF
	xorwf	MINUTOS,W	;Si son 0xFF, Z=1. Si no son 0xFF, Z=0
	btfss	STATUS,Z	;Miro Z
	return			;Si no son 0xFF retorno
	clrf	MINUTOS		;Si son 0xFF, pongo los MINUTOS a 0
	return			;y luego retorno
	
;Fin del subprograma RESTA
	
;******************************************************************************
;*	Programa de tratamiento de la interrupción generada por TMR1,	      *
;*	que se producirá cada 0,1s si está TMR1 en marcha		      *
;******************************************************************************

TEMPO   
;para salvar el contexto no podemos emplear la instrucción MOVF ya que afecta 
;al registro STATUS,para evitarlo empleamos la instrucción SWAPF	
        movwf 	W_tmp       	;Salvamos el registro W
        swapf 	STATUS,W    	;el registro STATUS "girado" en W
	bcf	STATUS,RP0	;Aseguramos el paso al banco 0
	bcf	STATUS,RP1
        movwf 	STATUS_tmp  	;Guardamos en el banco 0
	movf	PCLATH,W	;Salvamos también PCLATH
	movwf	PCLATH_tmp	
	
	;Vamos a determinar el motivo de la interrupción: TMR1 ó TMR2
	btfsc 	PIR1,TMR1IF 	;Comprobamos el flag TMR1IF
        goto	INT_TMR1        ;si está a 1, vamos a esa parte de código
	btfss	PIR1,TMR2IF	;Si no fue TMR1IF, comprobamos TMR2IF
	goto	RECUPERAR	;si no era ninguno, vamos a recuperar el contexto
	incf	DESBORDA_TMR2	;Si fue TMR2IF, se suma 1 al contador de desbordamientos
	bcf	PIR1,TMR2IF	;Se pone el flag a 0 por software
	goto	RECUPERAR	;y se recupera el contexto

INT_TMR1
	movlw	0x3C		;Si está a 1 pasó una décima. Precargamos la 
	movwf	TMR1H		;parte alta de TMR1: TMR1H con 0x3C
	movlw	0xB0		;Precargamos la parte baja: TMR1L
	movwf	TMR1L		;con 0xB0 para contar 0.1s de nuevo
	call	RESTA		;Llamada a resta para disminuir el tiempo en una decima
	bcf	PIR1,TMR1IF	;ponemos a cero el flag para la sig. interrupción	
RECUPERAR
;Para recuperar los registros salvados no podemos usar MOVF porque modifica a STATUS, 
;para evitarlo usamos la instrucción SWAPF
	movf	PCLATH_tmp,W	;Recuperamos PCLATH
	movwf	PCLATH
	swapf 	STATUS_tmp,W 	;Recuperamos el registro STATUS con un SWAPF
        movwf 	STATUS
	swapf 	W_tmp,F		;Recuperamos también el W con dos SWAPF
	swapf 	W_tmp,W
         
        retfie               	;Retorno del programa de tratamiento de
				;la interrupción	

;Fin del PTI
				
;******************************************************************************
        END                  	;fin del fichero
				
	
	    
	
	
	
	
