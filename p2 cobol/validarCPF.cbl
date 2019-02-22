                     program-id. validarCPF as "validarCPF".

       environment division.
       configuration section.
       
       special-names.
           decimal-point is comma.
           
       input-output section.

       data division.
       working-storage section.
       01 dados-CPF.
              02 digitosCPF. 
                 03 dig1  pic 9(1).
                 03 dig2  pic 9(1).
                 03 dig3  pic 9(1).
                 03 dig4  pic 9(1).
                 03 dig5  pic 9(1).
                 03 dig6  pic 9(1).
                 03 dig7  pic 9(1).
                 03 dig8  pic 9(1).
                 03 dig9  pic 9(1).
                 03 dig10 pic 9(1).
                 03 dig11 pic 9(1).
       01 cod-cliente redefines dados-CPF pic 9(11).
       
       01 mensagens.
           02 mensa1 value "CPF invalido".
           02 mensa2 value "Campo nao pode ser vazio ou igual a zero".
       
       01 variaveis-auxiliares.
           02 limpa-mensagem pic x(40).
              
       01 dados-ControleCPF.
      *numero de digitos
          02 a pic 9(04) value 11.
      *resulatdo da primeira soma
          02 b pic 9(04).
      *resultado da segunda soma
          02 c pic 9(04).
      *resto da divisão
          02 d pic 9(02).
          02 aux pic 9(02).
          02 auxCPF pic 9(11).
          02 auxdig pic 9(01).
              
       01 dados-editados.
          02 cod-cliente-e pic zzz.zzz.zzz/z9.
          
       linkage section.
** section para receber dados externos     
           01 ls-cod-cliente pic 9(11).
           
       procedure division using ls-cod-cliente.
       
       entradaCliente.
           accept cod-cliente-e at 1624.
           move cod-cliente-e to cod-cliente.
           if cod-cliente = 0 
               display mensa2 at 2001
               perform entradaCliente 
           else 
               display limpa-mensagem at 2001 
               perform valida-cpf.
           
       valida-cpf.
           move cod-cliente to auxCPF.
           perform validaCPFDigito1.
           if (dig10 = auxdig)
               perform validaCPFDigito2
           else
               display mensa1 at 2001
               perform entradaCliente.
           
       validaCPFDigito1.
          move dig10 to auxdig.
          compute b = (dig1*1)+(dig2*2)+(dig3*3)+(dig4*4)+(dig5*5)
          +(dig6*6)+(dig7*7)+(dig8*8)+(dig9*9).
          divide a into b giving aux remainder d.
          if (d > 10)
               move 0 to d.
          move d to dig10.
           
       validaCPFDigito2.
          compute c = (dig1*0)+(dig2*1)+(dig3*2)+(dig4*3)+(dig5*4)
          +(dig6*5)+(dig7*6)+(dig8*7)+(dig9*8)+(dig10*9).
          divide a into c giving aux remainder d.
          if (d > 10)
               move 0 to d.
          move d to dig11.
          if cod-cliente <> auxCPF
                display mensa1 at 2001
                perform entradaCliente
                
          else
               move cod-cliente to ls-cod-cliente.
               move zeros to cod-cliente, cod-cliente-e.
      *move zeros para limpar se nao continua armazenado pois é call 
      * estatico
               exit program.
      * exit program retorna para o programa que chamou, somente usado 
      * no subprogram
      * go back tambem retorna para o programa que chamou, entretanto se
      * for colocado no main, termina o programa e devolve o controle
      * para o S.o similar ao stop run
        
       end program validarCPF.


