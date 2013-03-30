/*
 * Compute the "binary length" of an integer:
 * the number of 1 bits in x
 */
unsigned int blength(unsigned int x)
{
	unsigned int s=0;
	while(x){
		s += x & 1;
		x >>= 1;
	}
	return s;
}
