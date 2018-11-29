## The Remote Protocol for Santorini 


### Connecting 

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
[[ optional: 
	     |                ||            |
	     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>| playing-as: ]]
	     |                ||            |   name

```

#### Message Formats

| message	    |  format	   | interpretation  |
| ----------------- | ------------ | ---------------------- |
| registration |       Name |	   the name of this player |
| playing as | 	      PA |	   a tuple with a modified name for this player |

A `PA` is a JSON array with two elements: `["playing-as" Name]`

### Running a Tournament

```
             .   .  .         ||            .   .     . 
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
```

#### Message Formats

| message                    | format	          | interpretation				   |
| -------------------------- | ------------------ | ---------------------------------------------- |
| other   		      | Name	  	  | the name of the opponent for the next game     |
|			      | 		  | |
| worker placement	      | Placement       | the list of workers placed			  |
| place		      | Place	  	  | the next worker position for this player	  |
|  			      | 		  | |
| take turn		      | Board		  | the current state of the board		  |
| action		      | Action	  | the action that this player wishes to take	  |

### End of Tournament Information 

```
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

#### Message Formats

| message                   | format	          | interpretation  |
| ------------------------- | ---------------------- | ---------------------- |
| informing players         | Results    	  | the results of a tournament  |

A _Results_ is a JSON array of _EncounterOutcome_s. 
