#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include "useful.h"
#define L 512
#define p 128
#define q 2
#define m 5

char Switch(uint8_t base,int8_t Full)
{
	if(Full)
	switch(base)
      {
		 case 0:
			  return '.';
			  break;
		 case 1:
			  return ',';
			  break;
		 case 2:
			  return '<';
			  break;
		 case 3:
			  return '>';
			  break;
		 case 4:
			  return '+';
			  break;
		 case 5:
			  return '-';
			  break;
		 case 6:
			  return '[';
			  break;
		 case 7:
			  return ']';
			  break;
		 default:
			  printf("UNrecognizable");
			  return false;
      }	
	else
		switch(base)
      {
		 case 0:
			  return '*';
			  break;
		 case 1:
			  return '^';
			  break;
		 case 2:
			  return 'v';
			  break;
		 case 3:
			  return '>';
			  break;
		 case 4:
			  return '+';
			  break;
		 case 5:
			  return '-';
			  break;
		 case 6:
			  return '[';
			  break;
		 case 7:
			  return ']';
			  break;
		 default:
			  printf("UNrecognizable");
			  return false;
}
}
void PrintInfo(chromosome* individual,int ret,int MAX)
{
	int i;
	/*chromosome * temp;
	temp=individual->ptR;
	printf("%2d**%2d  ",individual->name,temp->name);
	printf("V%2d ",individual->validity);
	printf("E%2d ",individual->endornot);
	printf("p %-4d ",individual->ptr);
	printf("P%2d ",individual->Ptr);
	printf("S%-3u ",individual->score);
	printf("A %-4d  ",individual->tape[C(0)]);*/
	//printf("%-8s",individual->info);
	/*for(i=0;i<MAX-8;++i)
			printf("    ");*/
	putchar(' ');
	printf("%3s",individual->info);
	if(ret)
		putchar('\n');
}
void PrintGene(chromosome* individual,int MAX,uint8_t here,int ret)
{
	int i,j,sorryptr;
	char output;
	uint8_t Base=0;
	sorryptr=individual->ptr;
	individual->ptr=-1;
	if (here==individual->name)
		printf(">");
	else
		printf(" ");
		//printf("%2d",individual->name);
	printf("%1X: ",individual->name);	
	for(i=0;i<MAX;++i)
	{		
		Base=Merge(GetBase(individual,individual->gene.DNA,true,false));		
		output=Switch(Base,true);
		printf("%c ",output);
		if(output=='.')
		{	
			++i;
			j=MAX-i;
			for(i=0;i<j;++i)
			{
				printf("%2x",individual->gene.DNA[i]);				
			}
			break;
		}
	}
	individual->ptr=sorryptr;
	if(ret)
		putchar('\n');
}
void PrintAllGene(int MAX,int8_t ret)
{
	int i=0;
	for(i=0;i<pop;++i)
	{
		PrintGene(&population[i],MAX,0,false);
		printf("%3s",population[i].death);
		printf("%2d",population[i].Ptr);
	}
	if(ret)
		putchar('\n');
}
void PrintTape(chromosome* individual,int MAX,uint8_t here,int ret)
{
	int i,sorryptr;
	char output;
	uint8_t Base=0;
	sorryptr=individual->ptr;
	individual->ptr=-1;
	if (here==individual->name)
		printf("%2d",individual->name);
	else
		printf("  ");
	printf("tape: ");
	for(i=0;i<MAX;++i)
	{
		Base=Merge(GetBase(individual,&(individual->tape[L/2]),true,true));
		output=Switch(Base,false);
		printf("%c   ",output);
	}
	individual->ptr=sorryptr;
	if(ret)
		putchar('\n');
}
void PrintTapeNum(chromosome* individual,int MAX,uint8_t here,int ret)
{
	int i;
	char output;
	int8_t Base=0;
	if (here==individual->name)
		printf("%2d",individual->name);
	else
		printf("  ");
	printf("tape: ");
	for(i=0;i<MAX;++i)
	{
		printf("%-4d",individual->tape[C(i)]);
	}
	/*putchar('\n');
	for(i=-MAX;i<0;++i)
	{
		printf("%4d",individual->tape[C(i)]));
	}*/
	if(ret)
		putchar('\n');
}
void PrintPTR(int ptr,int MAX,int ret)
{
	int i;
	printf("  -");
	for(i=0;i<=ptr&&i<MAX;++i)
			printf("----");
	if(ptr>=MAX)
		printf("二-  ");
	else if(ptr<0)
		printf("---二");
	else
		printf("|^|  ");
	if(ptr<0)
		ptr=-1;
	for(i=1;i<MAX-ptr;++i)
			printf("    ");
	if(ret)
		putchar('\n');
}
void PrintIndividual(chromosome* individual,uint8_t here)
{
	/*PrintInfo(individual,true,8);
	PrintGene(individual,8,here,true);
	PrintPTR(individual->ptr,8,true);
	PrintTapeNum(individual,8,here,true);
	PrintPTR(individual->Ptr,8,true);*/

}
void PrintPopulation(int generation,int MAX,uint8_t here,int8_t ret,char com[])
{
	chromosome *population1;
	int i;
	//population1=population[here].ptR;
	printf("%5d",generation);
	for(i=0;i<pop;++i)
	{
		PrintInfo(&population[i],false,MAX);
	}
	printf(" %-4s",com);
	//putchar('\n');
	//for(i=0;i<pop;++i)
	//{
	//	PrintGene(&population[i],MAX,population1->name,false);
	//}
	//putchar('\n');
	//if(ifnum)
	//{
	//	for(i=0;i<pop;++i)
	//	{
	//		PrintPTR(population[i].ptr,MAX,false);
	//	}
	//	putchar('\n');
	//	for(i=0;i<pop;++i)
	//	{
	//		PrintTapeNum(&population[i],MAX,here,false);
	//	}
	//	putchar('\n');
	//	for(i=0;i<pop;++i)
	//	{
	//		PrintPTR(population[i].Ptr,MAX,false);
	//	}
	//}
	//else
	//{
	//	for(i=0;i<pop;++i)
	//	{
	//		if(i==population1->name)
	//			PrintPTR(population[here].Ptr,MAX,false);
	//		else
	//			PrintPTR(population[i].Ptr,MAX,false);
	//	}
	//	putchar('\n');
	//	for(i=0;i<pop;++i)
	//	{
	//		PrintTape(&population[i],MAX,here,false);
	//	}
	//	putchar('\n');
	//	for(i=0;i<pop;++i)
	//	{
	//		PrintPTR(population[i].ptr,MAX,false);
	//	}
	//}
	if(ret)
		putchar('\n');
}
void PrintTapeChange(uint8_t res)
{
	int i;
	printf("%4d",generations);
	if(res>=250)
		printf("%4d",res);
	else
		printf("    ");
	for(i=0;i<pop;++i)
	{
		if(population[i].validity>=0&&((population[i].gene.DNA[0]>>4)/2))
			printf("[%2d]%2d| ",i,population[i].tcn);
		else
			printf("        ",i,population[i].tcn);
	}	
	putchar('\\n');
	putchar('\n');
}
void PrintGeneChange()
{
	int i;
	for(i=0;i<pop;++i)
	{
		printf("%2d:G| :%2d ",i,population[i].gcn);
	}
	putchar('\n');
}
void main()
{
    /*uint8_t pop;
    printf("Please input population:")
    //scanf("%u",&pop);
    pop=16;
    printf("population=%d",pop);
    int8_t tape[L]= {0};*/
	 chromosome*population2[17];
    chromosome*population3[16];
    chromosome*population4[17];
    chromosome*populationtape[16];
	 uint8_t res=0;
    int i;
    int j;    
	  char * s;
    //printf("Please input generations:\n");    
    generations=65535;
    //scanf("%d",&generations);	 
    //printf("generations=%d\n",generations);
	 LoadData();//加载数据

    InitialPopulation();//初始化、连接种群
	 PrintPopulation(65535,16,0,true,"inge");
    for(i=0; i<pop; ++i)
    {
        InitialTape(population[i].tape,2);//纸带初置
		  population[i].info[1]='.';
		  population[i].info[2]=',';
    }    	 
	 PrintPopulation(65535,16,0,true,"inta");
    for(generations; generations>=0; --generations)
    {
        Trans(population2,population4,8);//翻译、分类[存活；死亡]
		  s= tapess;
		  PrintPopulation(generations,16,0,false,s);
		  PrintAllGene(8,true);
		  //PrintTapeChange(res);
			if(!sortnum)
				Recreate();
        else
        {
			   //初始化所有个体常数				
            res=Influence(populationtape,population2);//环境反作用
				//PrintGeneChange();
				//if(populationtape[0]->name==populationtape[--sorttape]->name)
					CopyChromosome(populationtape[0],populationtape[--sorttape]);//优胜劣汰
				for(i=0;i<sortdead;i++)
					InitialChromosome(population4[i]);//死亡重生
				PrintPopulation(generations,16,0,true,"ric");
            ++which;
            if(which==sizeof(Answer))
                which=0;
            SetTapes(2);//重置所有纸带初值
				//重置优胜体常数
				InitialSorts(population2);
				//PrintPopulation(generations,16,0,true,"rested all tapes and rightsome cons");
        }		  
		  //PrintPopulation(generations,16,0,true,"rest rightsome constant");
        InitialSorts(population4);
    }
		  printf("\nhere1");
		  getchar();
            /*if(!ExpressAndTest(population[i],tape,limittime))
                population[i]->score=255-abs(tape[C(0)]-Answer[j]);
            else
                population[i]->score=0;
    }
    /*
    if(i==0)
        {
            population[i]->left=&(population[pop-1]->gene.DNA);
        population[i]->right=&(population[i+1]->gene.DNA);
        }
                if(i==pop-1)
                {

                }

    int i=0,LONG=0;
    int8_t tape[L]= {0};
    uint8_t Array[128]=">++]<<-.";
    //"-><[>-<+++]->>++++[++++++++++++++++++<+]>---+-+++++++++++>+<><+[+><><>++++++++++-<-++++[++[--------+++------],-----]].";
    //"++++++++++[[>+++++++>++++++++++>+++>+<<<<-]]>++>+++++++++++>++<<+++++++++++++++>+++-------------->+>.";
    chromosome test;
    LONG=sizeof(Array);

    printf("Array\n");
    PrintArray(Array,LONG);

    Brain_to_DNA(Array,LONG);

    printf("Array\n");
    PrintArray(Array,LONG);

    InitialChromosomeByDoubletArray(&test,Array,LONG);

    printf("test.gene.DNA\n");
    PrintArray(test.gene.DNA,LONG);

    ExpressAndTest(&test,tape,30);

    printf("tape\n");
    PrintTape(tape,16);

    return 0;*/
}

void InitialChromosomeByDoubletArray(chromosome* individual,uint8_t Array[],int LONG)
{
    int i;
    for (i=0; i<LONG; ++i)
        individual->gene.DNA[i]=Array[i];
}
int Brain_to_DNA(uint8_t Array[],int LONG)
{
    int i,half;
    uint8_t Code,temp;
    for (i=0; i<LONG; ++i)
    {
        half=i/2;
        Code=Array[i];
        if(!(i-half-half))
            switch(Code)
            {
            case '.':
                Array[half]=(0<<4);
                break;
            case ',':
                Array[half]=(2<<4);
                break;
            case '<':
                Array[half]=(4<<4);
                break;
            case '>':
                Array[half]=(6<<4);
                break;
            case '+':
                Array[half]=(8<<4);
                break;
            case '-':
                Array[half]=(10<<4);
                break;
            case '[':
                Array[half]=(12<<4);
                break;
            case ']':
                Array[half]=(14<<4);
                break;
            default:
                printf("CodeUNrecognizable");
                return true;
            }
        else
        {
            switch(Code)
            {
            case '.':
                Array[half]+=0;
                break;
            case ',':
                Array[half]+=2;
                break;
            case '<':
                Array[half]+=4;
                break;
            case '>':
                Array[half]+=6;
                break;
            case '+':
                Array[half]+=8;
                break;
            case '-':
                Array[half]+=10;
                break;
            case '[':
                Array[half]+=12;
                break;
            case ']':
                Array[half]+=14;
                break;
            default:
                printf("CodeUNrecognizable");
                return true;
            }
            temp=0;
        }
    }
}
int LoadShape(chromosome* individual)
{
	int i,j,k,temp1,temp2;
	uint8_t Base;
	printf("\tinfo:\tname:%d\t\n",individual->name);
	printf("\tgene:\t");
	temp1=individual->ptr;
	temp2=individual->Ptr;
	for (i=2;i<32&&!(individual->endornot);++i)
	{
		Base=Merge(GetBase(individual,individual->gene.DNA,true,false));
		switch(Base)
      {
		 case 0:
			  individual->endornot=true;
			  putchar('.');
			  break;
		 case 1:
			  putchar(',');
			  break;
		 case 2:
			  putchar('<');
			  break;
		 case 3:
			  putchar('>');
			  break;
		 case 4:
			  putchar('+');
			  break;
		 case 5:
			  putchar('-');
			  break;
		 case 6:
			  putchar('[');
			  break;
		 case 7:
			  putchar(']');
			  break;
		 default:
			  printf("UNrecognizable");
			  return true;
      }	
		printf("\t");
	}
	printf("\n");
	k=1+individual->ptr;
	for(j=-1;j<k;++j)
	{
		printf("\t");
	}
	printf("^\n");

}

void PrintArray(uint8_t Array[],int LONG)
{
    int i=0;
    while(i<LONG)
    {
        printf("[%d]=%d\n",i,Array[i]);
        ++i;
    }
}
//void PrintTape(uint8_t Array[],int LONG)
//{
//    int i=0;
//    while(i<LONG)
//    {
//        printf("[%d]=%d\n",i-1,Array[C(i-1)]);
//        ++i;
//    }
//}













