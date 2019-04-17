#ifndef USEFUL_H
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
//#include "main.c"
#define USEFUL_H
#define N 512
#define true 1
#define false 0
#define L 512
#define SC 4096
//extern pop,population[pop];
uint8_t pop=16;
uint8_t sortnum=0;
unsigned int which=0;
uint8_t sortdead=0;
uint8_t sorttape=0;
int generations;
int8_t TestArray[SC][2];
int8_t Answer[SC];
char tapess[16];
//printf("Please input population:")
//scanf("%u",&pop);
//pop
//printf("population=%d",pop);
typedef union
{
    short DNA4_t[N];
    uint8_t DNA[N*2];
} GENE;
typedef struct
{
    GENE gene;
    int8_t tape[L];
    int ptr,Ptr,tempptr;
    void * left;
    void * right;
    void * ptR;
    int8_t validity;
    int8_t endornot;
    int8_t changed;
    uint8_t name;
	 uint8_t score;
	 uint8_t tcn;
	 uint8_t gcn;
	 char death[4];
	 char info[3];
}chromosome;

chromosome population[16];

void PrintIndividual(chromosome* individual,uint8_t here);
void PrintPopulation(int generation,int MAX,uint8_t here,int8_t ifnum,char com);
void PrintAllGene(int MAX,int8_t ret);

void LoadData()
{
    FILE *fp;
    int i;
    int8_t c[SC][3];

    if((fp=fopen("ppp.bin","rb"))==NULL)
    {
        printf("Failed to open the file!\n");
        exit(1);
    }
    fread(c,3,SC,fp);
    for(i=0; i<SC; ++i)
    {
        TestArray[i][0]=c[i][0];
        TestArray[i][1]=c[i][1];
        Answer[i]=5;
    }
    fclose(fp);
	 /*for(i=0; i<SC; ++i)
    {
        printf("Test[%d]=%d ",i,TestArray[i][0]);
        printf("Test[%d]=%d\n",i,TestArray[i][1]);
        printf("Answer[%d]=%2d\n",i,Answer[i]);
    }*/
}
void InitialConstant(chromosome* individual)
{
	individual->ptr=-1;
	//individual->Ptr=0;
	individual->tempptr=-1;
	individual->validity=0;
	individual->endornot=false;
	individual->changed=false;
	individual->info[0]=':';
	individual->gcn=0;
	individual->tcn=0;
}
void Initialps(chromosome* individual)
{
    individual->Ptr=0;
}
void Initialtp(chromosome* individual)
{
    if(individual->Ptr<0)
		 individual->Ptr=0;
}
void InitialAllConstant()
{
	int i;
	for(i=0; i<pop; ++i)
	{
		InitialConstant(&population[i]);
		Initialps(&population[i]);
	}
}
void InitialSomeConstant(chromosome *population2[],uint8_t num)
{
	int i;
    for(i=0; i<num; ++i)
	 {
		 InitialConstant(population2[i]);    
		 Initialtp(population2[i]);
	 }
}
void InitialChromosome(chromosome* individual)
{
    int i;
    InitialConstant(individual);
	 individual->info[1]='.';
    for (i=0; i<N; ++i)
        individual->gene.DNA4_t[i]=rand();
}
void RecreateGene(chromosome* individual)
{
	int i;
    for (i=0; i<N; ++i)
        individual->gene.DNA4_t[i]=rand();
}
void InitialPopulation()
{
    int i;
	 InitialChromosome(&population[0]);
    population[0].left=&population[pop-1];
    population[0].right=&population[1];
	 population[0].ptR=&population[0];
    for(i=1; i<(pop-1); ++i)
    {
        population[i].left=&population[i-1];
        population[i].right=&population[i+1];
		  population[i].ptR=&population[i];
		  population[i].name=i;
        if (i<(pop/2))
		  {
            InitialChromosome(&population[i]);
		  }
		  else
			  InitialConstant(&population[i]);
    }    
	 population[i].name=pop-1;
	 InitialConstant(&population[pop-1]);
    population[pop-1].left=&population[pop-2];
    population[pop-1].right=&population[0];
	 population[pop-1].ptR=&population[pop-1];
}
void InitialSorts(chromosome *populations[])
{
	int i=0;
    sortnum=0;	 
    sortdead=0;
	 for(i=0;i<pop;++i)
		 populations[i]=populations[16];
}

void CopyGene(chromosome*Toindividual,chromosome*Frindividual)
{
    int i;
    for (i=0; i<N; ++i)
	 {
        Toindividual->gene.DNA4_t[i]=Frindividual->gene.DNA4_t[i];
		  //Toindividual->name=Frindividual->name;
	 }
	 Toindividual->info[1]='_';
}
void CopyChromosome(chromosome*Toindividual,chromosome*Frindividual)
{
    int i;
    if(Toindividual!=Frindividual)
    {	
        CopyGene(Toindividual,Frindividual);
    }
}

void InitialTape(int8_t tape[],int LONG)
{
    int i;
    for (i=0; i<LONG; ++i)
        tape[C(i)]=TestArray[which][i];
	 sprintf(tapess,"%2X%2X",tape[C(0)],tape[C(1)]);
}
void InitialTapeZero(int8_t tape[],int LONG)
{
    int i;
    for (i=0; i<LONG; ++i)
        tape[i]=0;
}
void SetTape(int8_t tape[],int LONG)
{
    InitialTapeZero(tape,L);
    InitialTape(tape,LONG);
}
void SetTapes(int LONG)
{	
    int i=0;
        for(i=0; i<pop; ++i)
		  {
			  SetTape(population[i].tape,LONG);
			  population[i].info[2]=',';
		  }
}

int C(int i)
{
    return L/2+i;
}
uint8_t RetBase(int PTR,uint8_t DNA[],chromosome* individual)
{
    if(!(individual->ptr)-PTR-PTR)
    {
        return DNA[PTR]>>4;
    }
    else
    {
        return DNA[PTR]&15;
    }
}
uint8_t RetBaseFull(uint8_t DNA[],chromosome*individual)
{
    return DNA[individual->ptr]&15;
}
uint8_t GetBase(chromosome* individual,uint8_t DNA[],int direction,int Full)
{
    JudgeDirection(individual,direction);
    if (Full)
    {
        return RetBaseFull(DNA,individual);
    }
    else
    {
        int PTR=(individual->ptr)/2;
        return RetBase(PTR,DNA,individual);
    }
}
int JudgeDirection(chromosome* individual,int direction)
{
    if(direction)
        return ++(individual->ptr);
    else
        return --(individual->ptr);
}
uint8_t Merge(uint8_t base)
{
    return base/2;
}

int Translate(chromosome* individual)
{
    int sta=1;
    uint8_t Base;
    Base=Merge(GetBase(individual,individual->gene.DNA,true,false));
    if(individual->tempptr==individual->ptr)
    {
        individual->validity=-1;
		  sprintf(individual->death,"%s","stp");
        return true;
    }
    individual->tempptr=individual->ptr;
    switch(Base)
    {
    case 0:
        individual->tape[C(0)]=individual->tape[C(individual->Ptr)];
        individual->endornot=true;
        return individual->endornot;
        break;
    case 1:
        break;
    case 2:
        --(individual->Ptr);
        break;
    case 3:
        ++(individual->Ptr);
        break;
    case 4:
        ++(individual->tape[C(individual->Ptr)]);
		  ++individual->tcn;
		  individual->info[2]='+';
        break;
    case 5:
        --(individual->tape[C(individual->Ptr)]);
		  ++individual->tcn;
		  individual->info[2]='+';
        break;
    case 6:
        ++(individual->validity);
        if (!(individual->tape[C(individual->Ptr)]))
        {
            while(!(Base==7&&sta==0))
            {
                Base=Merge(GetBase(individual,individual->gene.DNA,true,false));
                if(Base==6)
                {
                    ++sta;
                    ++(individual->validity);
                }
                if(Base==7)
                {
                    --sta;
                    --(individual->validity);
                }
                if(Base==0)
                {
                    individual->validity=-1;
						  sprintf(individual->death,"%s","mt.");
                    break;
                }
                if(individual->ptr>=2*N)
                {
                    individual->validity=-1;
						  sprintf(individual->death,"%s","byG");
                    break;
                }
            }
        }
        sta=0;
        break;
    case 7:
        --(individual->validity);
        if ((individual->tape[C(individual->Ptr)]))
        {
            while(!(Base==6&&sta==0))
            {
                Base=Merge(GetBase(individual,individual->gene.DNA,false,false));
                if(Base==7)
                {
                    ++sta;
                    --(individual->validity);
                }
                if(Base==6)
                {
                    --sta;
                    ++(individual->validity);
                }
                if(individual->ptr<0)
                {
                    individual->validity=-1;
						  sprintf(individual->death,"%s","ov0");
                    break;
                }
            }
        }
        sta=0;
		  if(individual->validity<=0)
			  sprintf(individual->death,"%s","dgs");
        break;
    default:
        printf("UNrecognizable");
        return true;
    }
    individual->endornot=false;
    return individual->endornot;
}
void Trans(chromosome *population2[],chromosome *population4[],int MAX)
{
    int i;
    int j;
	 InitialAllConstant();
	 for(i=0;i<pop;++i)
	{
		sprintf(population[i].death,"%s","ulm");
		/*population[i].ptr=population[i].Ptr-population[i].ptr;
		population[i].Ptr=population[i].Ptr-population[i].ptr;
		population[i].ptr=population[i].ptr+population[i].Ptr;*/
		//////SO IMPORTANT HERE!!!!!//////
	}
    for(j=0; j<MAX; ++j)
    {
        for(i=0; i<pop; ++i)
        {
            if((population[i].validity>=0)&&!(population[i].endornot))
				{
                Translate(&population[i]);		
					 population[i].info[1]=' ';
				}
				else if(population[i].validity>=0&&((population[i].gene.DNA[0]>>4)/2)&&!(population[i].changed))
            {
                population[i].score=255-abs(population[i].tape[C(0)]-Answer[which]);
                population2[sortnum++]=&population[i];
                population[i].changed=true;	
					 population[i].info[0]=';';
					 population[i].info[1]='=';
					 population[i].info[2]='*';
					 sprintf(population[i].death,"%s","   ");
            }
				else if(population[i].validity<0&&!(population[i].changed))
            {
                population4[sortdead++]=&population[i];
                population[i].changed=true;
					 population[i].info[0]=' ';
					 population[i].info[1]=' ';
					 population[i].info[2]=' ';
            }	
				else if(!((population[i].gene.DNA[0]>>4)/2)&&!(population[i].changed))
				{
					population[i].info[1]='.';
					sprintf(population[i].death,"%s","zr0");
					population[i].changed=true;
				}
        }
    }
}

uint8_t FindCodeAdd(chromosome *population1,int i)
{
    int half;
    uint8_t Base;
    half=i/2;
    if(!(i-half-half))
    {
        population1->gene.DNA[half]+=32;
		  Base=population1->gene.DNA[half];
		  population1->info[1]='-';
        return Base>>4;
    }
    else
    {
        Base=population1->gene.DNA[half];	
		  Base=Base&0xF0;
		  population1->gene.DNA[half]+=2;
		  population1->gene.DNA[half]=population1->gene.DNA[half]&15;
		  population1->gene.DNA[half]+=Base;
		  population1->info[1]='-';
        return population1->gene.DNA[half]&15;
    }
}
uint8_t FindCodeSub(chromosome *population1,int i)
{
    int half;
    uint8_t Base,base;
    half=i/2;
    if(!(i-half-half))
    {
        population1->gene.DNA[half]-=32;	
		  Base=population1->gene.DNA[half];
		  ++population1->gcn;  
		  population1->info[1]='-';
        return Base>>4;
    }
    else
    {
        Base=population1->gene.DNA[half];		  
		  Base=Base&0xF0;
		  population1->gene.DNA[half]-=2;
		  population1->gene.DNA[half]=population1->gene.DNA[half]&15;
		  population1->gene.DNA[half]+=Base;
		  population1->info[1]='-';
        return population1->gene.DNA[half]&15;
    }
}
uint8_t FindCode(uint8_t Array[],int i)
{
    int half;
    half=i/2;
    if(!(i-half-half))
        return Array[half]>>4;
    else
        return Array[half]&15;
}

int Transtape(chromosome *population2)
{	
	chromosome *population1;
    int sta=1;
    uint8_t Base;
    Base=Merge(GetBase(population2,(population2->tape)+L/2,true,true));
    if(population2->tempptr==population2->ptr)
    {
        population2->validity=-1;
        return true;
    }
    population2->tempptr=population2->ptr;
	 --(population2->score);
    if(!(population2->score))
    {
        population2->validity=-1;
        return true;
    }
    switch(Base)
    {
    case 0:
        break;
    case 1:
        population1=population2->ptR;
        population2->ptR=population1->left;
        break;
    case 2:
		 if(population2->ptr>1)
			 --(population2->Ptr);
		 break;
    case 3:
        ++(population2->Ptr);
        break;
    case 4:
        FindCodeAdd(population2->ptR,population2->Ptr);
        break;
    case 5:
        FindCodeSub(population2->ptR,population2->Ptr);
        break;
    case 6:
        ++(population2->validity);
        population1=population2->ptR;
        if(!(FindCode(population1->gene.DNA,population2->Ptr)))
        {
            while(!(Base==7&&sta==0))
            {
                Base=Merge(GetBase(population2,(population2->tape)+L/2,true,true));
                if(Base==6)
                {
                    ++sta;
                    ++(population2->validity);
                }
                if(Base==7)
                {
                    --sta;
                    --(population2->validity);
                }
                if(population2->ptr>255)
                {
                    population2->validity=-1;
                    break;
                }
            }
        }
        sta=0;
        break;
    case 7:
        --(population2->validity);
        population1=population2->ptR;
        if(FindCode(population1->gene.DNA,population2->Ptr))
        {
            while(!(Base==6&&sta==0))
            {
                Base=Merge(GetBase(population2,(population2->tape)+L/2,false,true));
                if(Base==7)
                {
                    ++sta;
                    --(population2->validity);
                }
                if(Base==6)
                {
                    --sta;
                    ++(population2->validity);
                }
                if(population2->ptr<0)
                {
                    population2->validity=-1;
                    break;
                }
            }
        }
        sta=0;
        break;
    default:
        printf("UNdefined");
        return true;
    }
}
uint8_t Influence(chromosome *populationtape[],chromosome *population2[])
{
    int i;
    int j;
	 int k;
	 int m;
    uint8_t res=0;
	 k=sortnum-1;
	 sorttape=0;
	 InitialSomeConstant(population2,sortnum);
	 //////SO IMPORTANT HERE!!!!!//////
    for(j=0; j<256; ++j)
    {
        for (i=k; i>=0; --i)
        {
            if (population2[i]->score>0)
            {
                if(population2[i]->validity>=0)
					 {
                    Transtape(population2[i]);
						  population2[i]->info[0]=';';
					 }
                else
                    --(population2[i]->score);
            }
            else if(!(population2[i]->changed))
				{
					populationtape[sorttape++]=population2[i];
					res=j;
					population2[i]->changed=true;
					m=sorttape-1;
				}
        }
    }
	 populationtape[m]->info[0]='#';
	 populationtape[m]->info[1]='#';
	 populationtape[m]->info[2]='#';
    return res;
}

void Recreate()
{
    int j;
	 //PrintAllGene(8,true);
    for (j=0; j<(pop/2); ++j)
        InitialChromosome(&population[j]);
	 printf("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tExtinction!!!\n\n");
}

#endif //USEFUL_H
