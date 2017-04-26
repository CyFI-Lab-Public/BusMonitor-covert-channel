/**
 * Memory Bus Covert Channel
 * Brendan Saltaformaggio
 * bdsaltaformaggio@gmail.com
 *
 * I am not responsible for this code or its use. :P
 *
 * Just compile with gcc on Linux. I haven't tested other compilers/systems.
 *
 * I do NOT provide any support for using or modifying
 * this code. So please do not ask!
 */
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <unistd.h>
#include <stdint.h>
#include <sched.h>

//#define SAME_HOST
#define DEBUG
#define DEBUG_L2

/**
 * Output Funcs
 */
#define OUT(fmt, args...)       do { printf("[CovChan +] "fmt"\n", ## args); } while(0)
#define ERROUT(fmt, args...)    do { printf("[CovChan !] "fmt"\n", ## args); } while(0)

#ifdef DEBUG
 #define DBG(fmt, args...)      do { printf("[CovChan -] "fmt"\n", ## args); } while(0)
#else 
 #define DBG(fmt, args...)      do {} while(0)
#endif // DEBUG

#ifdef DEBUG_L2
 #define DBG2(fmt, args...)     do { printf("[CovChan =] "fmt"\n", ## args); } while(0)
#else
 #define DBG2(fmt, args...)     do {} while(0)
#endif // DEBUG_L2


/* Forward Decl. */
unsigned long long setup_m_exotic( void );
unsigned long long setup_m_nocache( void );
char parse_args( int, char** );
void send( void );
void receive( void );
#ifdef SAME_HOST
int set_cpu_affinity( void );
#endif
void set_thresholds( void );
static inline void print_buffer( unsigned char* );

/* Globals */
 // a pointer to 32 bits that are misaligned and cross cache lines.
volatile uint32_t *m_exotic;
 // a pointer to a 64 bit  memory region to use for "uncached" moves.
volatile uint64_t *m_nocache;

 // buffer for sender
#define BUFF_HEADER "\xff\xff"
#define SIZE_OF_BUFF_HEADER (sizeof(BUFF_HEADER) - 1)
#define HEADER_EVERY_X_BYTES 8
#define INPUT_SIZE	(1026)
#define HEADER_BYTES_FOR_INPUT (((INPUT_SIZE/HEADER_EVERY_X_BYTES)+1) * SIZE_OF_BUFF_HEADER)
#define BUFF_SIZE ((INPUT_SIZE+HEADER_BYTES_FOR_INPUT)*16) // 2 1/2 bits for every bit of input.
unsigned char *buffer;
unsigned int input_size = 0;
unsigned int buffer_size = 0;

 // buffer size for receiver -
 //	 number of measurements to take before stopping.
#define REC_BUFFER_SIZE (BUFF_SIZE * 100)

 // thresholds experienced when sending/recv a 0
uint64_t threshold;

/* Prog. Args */
unsigned char arg_is_sender = 0;
uint64_t arg_delay = 0xFFFFF;
unsigned char arg_is_test = 0;

int main( int argc, char* argv[] )
{
	m_exotic = (uint32_t*)setup_m_exotic();
	m_nocache = (uint64_t*)setup_m_nocache();
	
	/* alloc an aligned buffer ... IDK if it helps? */
	if(posix_memalign((void**)&buffer, 4096, BUFF_SIZE) != 0)
		ERROUT("Cannot alloc buffer?!");

	if(parse_args(argc, argv) != 0)
		return -1;

#ifdef SAME_HOST
	if (set_cpu_affinity())
		ERROUT("set cpu");
#endif

	set_thresholds();

	DBG("Tresh: %ld", threshold);
	
	if( arg_is_sender )
	{
		struct timeval	start, end;
		double start_time_in_nano;
		double end_time_in_nano;
		double x;
		OUT("Sending");

		gettimeofday(&start, NULL);
		send();
		gettimeofday(&end, NULL);

		start_time_in_nano = start.tv_sec;
		start_time_in_nano *= 1000000000;
		start_time_in_nano += start.tv_usec;

		end_time_in_nano = end.tv_sec;
		end_time_in_nano *= 1000000000;
		end_time_in_nano += end.tv_usec;

		end_time_in_nano -= start_time_in_nano;
		x = input_size; // # of bytes in input
		OUT("Bytes:%f\tNanos:%f", x, end_time_in_nano); 
		end_time_in_nano /= 1000000000; // seconds
		OUT("Bytes/sec (Bps):%f", x / end_time_in_nano);
		OUT("bits/sec  (bps):%f", (x * 8) / end_time_in_nano);
	}
	else
	{
		receive();
	}
}

/**
 * Print the given buffer (in hex and chars).
 */
static inline void print_buffer(unsigned char* buf)
{
	int i;
	for(i = 0; i < strlen(buf); i++)
		OUT("0x%X (%c)", buf[i], buf[i]);
}

#ifdef SAME_HOST
/**
 * Make sure the sender and receiver are not on the same CPU!
 */
int set_cpu_affinity()
{
	cpu_set_t mask; 
	CPU_ZERO(&mask); 

	if( arg_is_sender )
		CPU_SET(0, &mask); 
	else
		CPU_SET(1, &mask); 
	
	return sched_setaffinity(0,sizeof(mask), &mask);
}
#endif


/**
 * Setup the address of m_nocache.
 *
 * Return an unsigned long long that is the
 *  address of an uint64_t in valid memory, but
 *  on it's own cache line.
 */
unsigned long long setup_m_nocache( void )
{
	return (unsigned long long)malloc(sizeof(uint64_t));
}


/**
 * Setup the address of m_exotic.
 *
 * Return an unsigned long long that is the
 *  address of an uint64_t in valid memory that
 *  is misaligned and crosses 2 cache lines.
 */
unsigned long long setup_m_exotic( void )
{
/* Some ideas stolen from: http://www.alexonlinux.com/aligned-vs-unaligned-memory-access */
#define CACHE_LINE_SIZE 64

	unsigned long long addr;
	int err;

	if(posix_memalign((void**)&addr, CACHE_LINE_SIZE, CACHE_LINE_SIZE * 2) != 0)
		return 0;
	
	/* Now addr points to chunk of memory	*
	 * that is 2 cache lines big and 	*
	 * aligned to the beginning of the	*
	 * first cache line. Let's screw it up!	*/
	return addr + (CACHE_LINE_SIZE - 2); // 3 bytes of m_exotic on first cache line!
}

unsigned int extend(unsigned char *buf)
{
	unsigned int i, j, k, len = strlen(buf);
	uint8_t bits;

	input_size = len;
	
	bzero(buffer, BUFF_SIZE);	

	k = 0; // index in send buffer;
	for(i = 0; i < len; i++)
       	{ // i = index in input buffer
		if(i % HEADER_EVERY_X_BYTES == 0) // header every x bytes!
		{
			for(j = 0; j < SIZE_OF_BUFF_HEADER * 8; j++)
			{
				buffer[k] = 1;
				buffer[k+1] = 1;
				k += 2;
			}
		}

        	for(bits = 1; bits !=0; bits<<=1)
                {// for each bit in a byte
			if(buf[i] & bits)
			{
				buffer[k] = 1;
				buffer[k+1] = 1;
			}
			k += 2;
		}
	}

	return k;
}

/****************************** CODING!!! ***********************************/
// switch on data 0s	-------------------------v
#define data_switch(data, prev_signal)	(data == 0? !prev_signal : prev_signal)
#define clock_switch(prev_signal)	(!prev_signal)

void encode_buffer( void )
{
	int i;

	for(i = 0; i < buffer_size; i+= 2)
	{
		if(i % ((HEADER_EVERY_X_BYTES+SIZE_OF_BUFF_HEADER)*16) == 0)
                {
			buffer[i+1] = clock_switch(buffer[i]);
		}
		else
		{
			buffer[i] = data_switch(buffer[i], buffer[i-1]);
			buffer[i+1] = clock_switch(buffer[i]);
		}
	}
}

static inline unsigned char keep_switch_at(uint8_t *bits)
{
#define SIGS_NEEDED_TO_KEEP	(27)
#define OUT_OF			(30)
	int i;
	uint8_t	count = 0;

	for(i = 1; i < OUT_OF; i++)
	{
		if(bits[i] == bits[0])
			count++;
	}

	return count >= SIGS_NEEDED_TO_KEEP;
}

unsigned int next_switch(uint8_t *bits, uint64_t max_to_read)
{
	uint64_t i = 1;

	while(i < max_to_read)
	{
		if(bits[0] != bits[i] && keep_switch_at(&(bits[i])))
                {
			break; 
                }
		i++;
	}

	DBG("%d x %ld", bits[0], i);

	return i;
}

#define UPPER_LDELTA(x)	((2*(x))+((x)/2))
#define LOWER_LDELTA(x)	((2*(x))-((x)/2))
#define is_long_delta(x, short_delta)				\
	((LOWER_LDELTA(short_delta)) <= (x) && (x) <= (UPPER_LDELTA(short_delta)))

#define UPPER_SDELTA(x) ((x)+((x)/2))
#define LOWER_SDELTA(x) ((x)-((x)/2))
#define is_short_delta(x, short_delta)                  		\
        ((LOWER_SDELTA(short_delta)) <= (x) && (x) <= (UPPER_SDELTA(short_delta)))
	

void decode_buffer (uint8_t *bits )
{
	uint64_t j=0, next_sig, delta, short_delta;
	uint64_t index = 0, accu_j = 0;
	char *work_buffer = &buffer[0]; // do not clobber orig buffer ptr

	bzero(work_buffer, BUFF_SIZE);

	/*** Check for header ***/

	 // skip leading zeros
	do {
		index += next_switch(&bits[index], REC_BUFFER_SIZE - index);
        	if(index == REC_BUFFER_SIZE)
			goto out;
	} while (bits[index] == 0);
	
check_header_start:

	j = 0;

	// first a short 1
	delta = next_switch(&bits[index], REC_BUFFER_SIZE - index);
	if(delta == REC_BUFFER_SIZE - index)
		goto out;
	index += delta;

	short_delta = delta; // the short 1 is asserted for "short_delta" time (1/2 data bit).

	DBG("S Delta: %ld", short_delta);

	work_buffer[j] = 1;
	j++;

	/* next a "long" zero.... */
	delta = next_switch(&bits[index], REC_BUFFER_SIZE - index);
        if(delta == REC_BUFFER_SIZE - index)
        	goto out;
        index += delta;

	if(is_long_delta(delta, short_delta))
        {
		short_delta = (short_delta + (delta/2)) / 2;
                work_buffer[j] = 1; // long delta means signal wasn't switched!
                j++;
        }
	else if(is_short_delta(delta, short_delta))
	{	// sometimes the "short 1" before is a little long,
		// meaning that both of these were long.
		// Try to recover by adjusting the short delta.
		short_delta = ((short_delta/2) + (delta/2)) / 2;
		work_buffer[j] = 1; // long delta means signal wasn't switched!
		j++;
                DBG2("S Fix Delta: %ld! %ld", short_delta, j);

	}
        else    // Cannot be the header!
        {
                goto check_header_start;
        }

	/* Rest of the header ... */
	
	for(/*j*/; j < ((SIZE_OF_BUFF_HEADER * 8)); )
	{
		delta = next_switch(&bits[index], REC_BUFFER_SIZE - index);
		if(delta == REC_BUFFER_SIZE - index)
			goto out;
		index += delta;

		if(is_long_delta(delta, short_delta))
		{
			short_delta = (short_delta + (delta/2)) / 2;
			work_buffer[j] = 1; // long delta means signal wasn't switched!
			j++;
			DBG2("S Delta: %ld! %ld", short_delta, j);
		}
		else if(is_short_delta(delta, short_delta) && 
			j > ((SIZE_OF_BUFF_HEADER * 8)/2) && 	// more than half the header seen!
			bits[index] == 0) 			// we just saw a 1 asserted...
		{	// It is possible that we are at the end of the header
			// and just missed a few bits at the beginning...
			while(j < (SIZE_OF_BUFF_HEADER * 8))
			{
				work_buffer[j] = 1;
				j++;
			}
			j=0;
			goto handle_short_data;
		}
		else	// No long send? Cannot be the header!
		{
			goto check_header_start;
		}
	}

	j=0;

	/* If we have found enough long deltas in a row then
	   we have found the "constant part" of the header.
	   From here on we are finding "data":
	   If we see a long bit then we know a data 1, if we see
	   a short bit then we know a data 0 */

	for( ; j < BUFF_SIZE; j++)
	{
#ifdef DEBUG2
		if( (j+1) % 8 == 0)
			DBG2("------------------");
#endif
		if(j != 0 && (j % (HEADER_EVERY_X_BYTES * 8) == 0))
                	goto neither;

		
		delta = next_switch(&bits[index], REC_BUFFER_SIZE - index);
        	if(delta == REC_BUFFER_SIZE - index)
			goto out; // cannot tell if last bit was long or short!

		index += delta;
		
		if(is_short_delta(delta, short_delta))
		{
handle_short_data:
			work_buffer[j] = 0;

			short_delta = (short_delta + delta) / 2;

			/* Must see another short switch (to get to mid bit for this 0) */
			delta = next_switch(&bits[index], REC_BUFFER_SIZE - index);
			if(delta == REC_BUFFER_SIZE - index)
                        	goto out; // cannot tell if last bit was long or short!
			index += delta;

			if(!is_short_delta(delta, short_delta))
			{
				DBG2("incorrect long delta? %ld", delta);
				continue; // problem?
			}
			
			short_delta = (short_delta + delta) / 2;
		}
		else if(is_long_delta(delta, short_delta))
		{
			short_delta = (short_delta + (delta/2)) / 2;
			work_buffer[j] = 1;
			
		}
		else	// neither??
		{
neither:
			DBG2("%ld < %ld < %ld ???", LOWER_LDELTA(short_delta),
                                 delta, UPPER_LDELTA(short_delta));
			DBG2("Old j: %ld", j);
			while(j % 8 != 0)
				j++;
			DBG2("New j: %ld", j);
			work_buffer += j;
			accu_j += j;
			goto check_header_start;
		}
	}

out:
	accu_j += j;
	buffer_size = accu_j;
	DBG("BUFF%d:", buffer_size);
	return;
}


/**
 * Parse args and set program settings.
 */
char parse_args(int argc, char* argv[])
{
	unsigned char c;
	opterr = 0;

	bzero(buffer,BUFF_SIZE);
	
	while ((c = getopt (argc, argv, "thd:s:")) != -1)
		switch (c)
		{
		 case 't':
			arg_is_test = 1;
			break;
		 case 'h':
			OUT("MemBus Covert Channel");
			OUT(" -d<Delay>\t\tSpecify the Delay (frame time window) in Hex. Default 0xFFFFFFF.");
			OUT(" -s<Message>\tThis instance is a sender, please send Message into the Clouds.");
			return 3;
		 case 'd': /* Delay */
			sscanf(optarg, "%lX", &arg_delay);
			break;
		 case 's': /* Sender & Message */
			arg_is_sender = 1;
			if (strncmp (optarg,"00",2) != 0) // -s00x = special send
			{				  // otherwise -s<message>
#ifdef DEBUG
				print_buffer(optarg);
#endif
				buffer_size = extend(optarg);
				encode_buffer();
			}
			else if(optarg[2] == '1') // -s001 = junk send max
			{
				unsigned char my_buf[INPUT_SIZE];
				uint64_t i;
                                bzero(my_buf, INPUT_SIZE);
                                for(i = 0; i < INPUT_SIZE - 1; i++)
					my_buf[i] = 'H';// + (i % 26);
				my_buf[INPUT_SIZE - 1] = 0;
#ifdef DEBUG
                                print_buffer(my_buf);
#endif
                                buffer_size = extend(my_buf);
                                encode_buffer();
			}				// -s000 = null send.
			break;
		 case '?':
			ERROUT("Unknown option or missing arg for `-%c'.", optopt);
			OUT("-h for Help.");
			return 1;
		 default:
			return 0;
		}
}


/**
 * Perform the "send 0" memory operation for arg_delay clock ticks.
 *
 * Returns the average running time of the mem. op.  over all
 *  executions in clock ticks.
 */
static inline uint64_t uncached_until_timeout(void)
{
	register uint64_t *m_nocache_reg = m_nocache; // force ptr into a reg.
	register uint64_t total_time, diff, count = 1;

/* I am doing this in a MACRO so I dont have to write it twice. */
#define timed_uncache(time_elapsed)						\
	asm volatile(   "1:			\n\t"				\
			"rdtsc                  \n\t"				\
			"movq %%rdx, %%r14	\n\t" /* Time Safe */		\
			"movq %0, %%r13		\n\t"				\
			"movq (%1), %%rax	\n\t"				\
			"rdtsc                  \n\t"				\
			"cmpq %%rdx, %%r14	\n\t" /* Time Safe */		\
			"jne 1b			\n\t" /* Time Safe */ 		\
			"subq %%r13, %0		\n\t"				\
			: "=a"(time_elapsed)    /* outputs */			\
			: "r"(m_nocache_reg)	/* inputs  */			\
			: "memory","r13","r14","rdx")	/* clobber */

	timed_uncache(total_time);
	
	while(total_time < arg_delay) // gets set to 1 when timer goes.
	{
		timed_uncache(diff);
		total_time += diff;
		count++;
	}

	return total_time / count;
}


/**
 * Perform the "send 1" auton. memory operation for arg_delay clock ticks.
 *
 * Returns the average running time of auton. inst. over all
 *  executions in clock ticks.
 */
static inline uint64_t lock_mem_bus_until_timeout(void)
{
	register uint32_t *m_exotic_reg = m_exotic; // force ptr into a reg.
	register uint64_t total_time, diff, count = 1;
	register uint64_t delay = arg_delay + (arg_delay/2); 	// ONES must be asserted for longer
								// to be held as long as twos

/* I am doing this in a MACRO so I dont have to keep writing it. */
#define timed_lock(time_elapsed)	 					\
	asm volatile(  	"1:			\n\t"				\
			"rdtsc                  \n\t"				\
			"movq %%rdx, %%r14	\n\t" /* Time Safe */		\
			"movq %0, %%r13		\n\t"				\
			"lock			\n\t"				\
			"incl (%1) 	 	\n\t"				\
			"rdtsc                  \n\t"				\
			"cmpq %%rdx, %%r14	\n\t" /* Time Safe */		\
			"jne 1b			\n\t" /* Time Safe */		\
			"subq %%r13, %0		\n\t"				\
			: "=a"(time_elapsed)    /* outputs */			\
			: "r"(m_exotic_reg)   	/* inputs  */			\
			: "memory","r13","r14","rdx")	/* clobber */	

	timed_lock(total_time);

	while(total_time < delay)
	{
		timed_lock(diff);
		total_time += diff;
		count++;
	}

	return total_time / count;
}

void send( void )
{
	register unsigned int i;
	register uint64_t avg_time;
#ifdef DEBUG
	uint16_t time[BUFF_SIZE];
#endif
	for(i = 0; i < buffer_size; i++) // each char in buffer
	{
		if( buffer[i] )
			avg_time = lock_mem_bus_until_timeout();
		else
			avg_time = uncached_until_timeout();
#ifdef DEBUG
			time[i] = (uint16_t)avg_time;
#endif
	}

#ifdef DEBUG
	for(i = 0; i < buffer_size; i++){
		if(i % 16 == 0)
			DBG("--------");
		DBG("%d\ttime %d", buffer[i], time[i]);
	}
#endif
}


void reassemble_buffer( void )
{
	unsigned char my_buf[BUFF_SIZE];
	unsigned int i,j;
	unsigned char bit;

	bzero(my_buf, BUFF_SIZE);
	
	for(i = 0, j = 0; i < buffer_size; j++)
	{			
		for(bit = 1; bit != 0; bit<<=1, i++)
		{
			if(buffer[i])
				my_buf[j] |= bit;
		}
	}
	memcpy(buffer, my_buf, BUFF_SIZE);	

}


void receive( void )
{
    uint64_t buff_char;
	uint8_t *bits = malloc(REC_BUFFER_SIZE);
	register uint64_t thresh = threshold;

	if(!bits)
	{
		ERROUT("mem fail");
		return;
	}

	for(buff_char = 0; buff_char < REC_BUFFER_SIZE; buff_char++)
        {
		bits[buff_char] = 0; // mainly to force it into mem...
	}

	OUT("Listening.");
	
    for(buff_char = 0; buff_char < REC_BUFFER_SIZE; buff_char++)
    {
        bits[buff_char] = (lock_mem_bus_until_timeout() > thresh);
    }
       
	OUT("Decoding.");
	decode_buffer(bits);
	reassemble_buffer();
	print_buffer(buffer);
	if(arg_is_test)
	{
		float wrong_chars = 0;
		unsigned int size = buffer_size / 8;
		if(size != INPUT_SIZE-1)
			OUT("Size not accurate! %d",size);

		for(buff_char = 0; buff_char < size; buff_char++)
		{
			if(buffer[buff_char] != 65)
				wrong_chars++;
		}
		OUT("Wrong chars: %f", wrong_chars);
		wrong_chars += (INPUT_SIZE - 1) - size;
		OUT("%% = %f", (wrong_chars/(INPUT_SIZE-1))*100);
	}
}

void set_thresholds()
{
#define NUM_OF_TRIALS 2000
	register uint16_t i;
	register uint64_t sum=0;
	uint16_t times[NUM_OF_TRIALS];

	if(arg_is_sender)
	{
		; // nada;
	}
	else // receiver
	{
		for(i = 0; i < NUM_OF_TRIALS; i++)
		{
			times[i] = lock_mem_bus_until_timeout();
			sum += times[i];
		}
	}
	threshold =  sum / NUM_OF_TRIALS;
	threshold += (threshold / 3);
}
