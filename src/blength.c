unsigned int blength(unsigned int x)
{
	unsigned int s=0;
	while(x){
		s += x&1;
		x>>=1;
	}
	return s;
}
