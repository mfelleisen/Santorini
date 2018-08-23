
	    server                
			      ||                                  
	     |                ||          client a                     
	     |                ||            |                      
	     |<-----------------------------| tcp connect 
	     |                ||            |                      
	     |                ||            | a
	     |                ||        pa  |---+ player new         
	     |                ||            |   |                     
	     |                ||            |   | pa, tcp 
	     |                ||            |---------+ rpAdmin new 
	     |       a        ||            |   |     |
	     |<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~| register: by name 
	     |                ||            |   |     |
	     |                ||            |   |     |
	     | a, tcp new     ||            |   |     |
	rpp1 |---+ rpPlay     ||            |   |     |
	     |   |            ||            |   |     |
	     .   .  .         ||            .   .     .
	     .   .  .         ||            .   .     .           ...
	     |       b        ||            |   |     |            |
	     |<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~| register client b
	     |   |            ||            |   |     |            |
	     |   |            ||            |   |     |           ...
	     | b, tcp new     ||            |   |     |           
	rpp2 |------+ rpPlay  ||            |   |     |           
	     |   |  |         ||            |   |     |           
	     .   .  .         ||            .   .     .
	     .   .  .         ||            .   .     .
	     .   .  .         ||            .   .     .   
tournament:  .   .  .         ||            .   .     .   
    new      |   |  |         ||            |   |     |           
    rp1, rp2 |   |  |         ||            |   |     |           
    +--------|   |  |         ||            |   |     |           
    |        |   |  |         ||            |   |     |           
    |        |   |  |         ||            |   |     |           
    |----------->|  |         ||            |   |     | other 
    |        |   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>| tcp other 
    |        |   |  |         ||            |<--------| other 
    |        |   |  |         ||            |   |     |           
    .        .   .  .         ||            .   .     .
    .        .   .  .         ||            .   .     .
    |        |   |  |         ||            |   |     |           
    |        |   |  |         ||            |   |     |           
    |----------->|  |         ||            |   |     | placement 
    |        |   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>| tcp placement 
    |        |   |  |         ||            |   |     |
    |        |   |  |         ||            |<--------| placement 
    |        |   |  |         ||            |------->>| return place
    |        |   |  |         ||            |   |     |
    |        |   |<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~| tcp place return
    |<<----------|  |         ||            |   |     | return place 
    |        |   |  |         ||            |   |     |           
    .        .   .  .         ||            .   .     .
    .        .   .  .         ||            .   .     .
    .        .   .  .         ||            .   .     .
    |----------->|  |         ||            |   |     | take-turn 
    |        |   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>| tcp take-turn 
    |        |   |  |         ||            |   |     |
    |        |   |  |         ||            |<--------| take-turn 
    |        |   |  |         ||            |------->>| return action 
    |        |   |  |         ||            |   |     |
    |        |   |<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~| tcp action return
    |<<----------|  |         ||            |   |     | return action 
    |        |   |  |         ||            |   |     |           
    .        .   .  .         ||            .   .     .
    .        .   .  .         ||            .   .     .
    .        .   .  .         ||            .   .     .
    |----------->|  |         ||            |   |     | inform
    |        |   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>| tcp inform 
    |        |   |  |         ||            |   |     |
    |        |  ======        ||            |<--------| inform
    |        |                ||            |   |    ===
    |        |                ||            |   |
    .        .                ||            |   |     
    +        .                ||            |   . 
	     =                ||            =   = 



message formats: 
 registration of a player | a string from client (tournament admin) to server 
 other        	   	  | a string from server (referee) to client (admin)
 placements		  | a placement list from server to client 
 place 			  | a place spec from client to server 
 board			  | a board from server to client 
 action 		  | an action from server to client 
 results		  | a results list from server to client, done 
