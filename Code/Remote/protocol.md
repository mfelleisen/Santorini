```
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
```


|  message		  |  format    |	from        |       to                    |
| ----------------------- | ---------- | ------------------ | --------------------------- |
| registration of a player| a string   | client : tournament manager |  server            |
| 	       	    	  |   	       | 	  	     |	     |			  |
| other        	   	  | a string   | server : referee | client : remote manager |
| 	       	    	  |   	       | 	  	     |	     |			  |
| placements		  | a placement list | server : referee | client : remote manager |
| place			  | a place spec     | client : remote manager | server : referee |
| 	       	    	  |   	       | 	  	     |	     |			  |
| board			  | a board          | server : referee | client : remote manager |
| action 		  | an action        | client : remote manager | server : referee |
| 	       	    	  |   	       | 	  	     |	     |			  |
| results		  | a results list   | server : tournament manager | client : | remote manager |
