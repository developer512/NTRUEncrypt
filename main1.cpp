#include <iostream>
#include <string>
#include <ctime>

using namespace std;

// Char 16 to array
void charToArray16 (char a, short int *A)
{
	unsigned short int i = 1;
	char16_t b = a;
	
	for (short int k = 0; k < 16; k++)
	{
		A[k] = (i & b) >> k;
		i = i << 1;
	}
}

// Array to char 16
char array16ToChar (short int *A)
{
	unsigned short int i = 1;
	char a = 0;
	char16_t b = 0;
	
	for (short int k = 0; k < 16; k++)
	{
		A[k] = A[k] << k;
		b = b | (A[k] & i);
		i = i << 1;		
	}
	a = (char)(b);

	return a;
}

// Message convertation into binary array
void messageToArray (string message, short int *arr)
{
	short int A[16];

	for (int k = 0; k < message.length (); k++)
	{
		charToArray16 (message[k], A);
		for (short int l = 0; l < 16; l++)
			arr[l + k * 16] = A[l];
	}
}

// Array convertation into symbolic line
string arrayToMessage (short int *arr, short int arraySize)
{
	int N = arraySize / 16;
	char *m = new char[N + 1];
	short int A[16];
	string message;

	for (int k = 0; k < N; k++)
	{
		for (int l = 0; l < 16; l++)
			A[l] = arr[l + k * 16];
		m[k] = array16ToChar (A);
	}
	m[N]= '\0';
	message = string(m);

	return message;
}

// Get polinomial degree
short int polynomialDegree (short int *a, short int arraySize)
{
	short int k = arraySize - 1;
	while (k > -1)
	{
		if (a[k] != 0)
			break;
		k--;
	}

	return k;
}

// Get modular inverse number to number a
short int modularInverse(short int a, short int m)
{
	if (abs(a) >= m)
		a = a % m;
	if (a < m)
		a = a + m; 

	short int q, r, r1, r2, z, z1, z2;

	r1 = m;
	r2 = a;
	z1 = 0;
	z2 = 1;

	while (r2 > 0)
	{
		q = r1 / r2;
		r = r1 - q * r2;
		r1 = r2;
		r2 = r;
		z = z1 - q * z2;
		z1 = z2;
		z2 = z;
	}
	if (z1 < 0)
	{
		z1 = m + z1;
		return z1;
	}
	else
		return z1;
}

// Polynomial modulus reduction
void modulusReduction (short int *polynomial, short int arraySize, short int modulus, short int *reducedPolynomial)
{
	for (short int k = 0; k < arraySize; k++)
	{
		reducedPolynomial[k] = polynomial[k] % modulus;
		if (reducedPolynomial[k] < 0)
			reducedPolynomial[k] = reducedPolynomial[k] + modulus;
	}
}

// f(x) + g(x)
void polynomialSum (short int *polynomial1, short int *polynomial2, short int arraySize, short int *resultPolynomial)
{
	for (short int k = 0; k < arraySize; k++)
		resultPolynomial[k] = polynomial1[k] + polynomial2[k];
}

// f(x) div x
void XDivision (short int *pol, short int arraySize, short int *resPol)
{
	short int *resultPolynomial = new short int [arraySize];

	memcpy(resultPolynomial, pol, arraySize * 2);
	if (polynomialDegree (resultPolynomial, arraySize) == (arraySize - 1))
	{
		for (short int k = 1; k < arraySize; k++)
			resultPolynomial[k - 1] = pol[k];
		resultPolynomial[arraySize - 1] = pol[0];
	}
	else
	{
		for (short int k = 1; k < arraySize - 1; k++)
			resultPolynomial[k - 1] = pol[k];
		resultPolynomial[arraySize - 2] = pol[0];
	}

	memcpy(resPol, resultPolynomial, arraySize * 2);

	delete [] resultPolynomial;
}

// f(x) * x
void XMultiplication (short int *pol, short int arraySize, short int *resPol)
{
	short int *resultPolynomial = new short int [arraySize];
	
	for (short int k = arraySize - 2; k > 0; k--)
		resultPolynomial[k] = pol[k - 1];
	resultPolynomial[0] = pol[arraySize - 2];
	resultPolynomial[arraySize - 1] = 0;

	memcpy(resPol, resultPolynomial, arraySize * 2);

	delete [] resultPolynomial;
}

// f(x) * a, a = const
void numberMultiplication (short int *pol, short int arraySize, short int a, short int *resPol)
{
	short int *resultPolynomial = new short int [arraySize];
	
	for (short int k = 0; k < arraySize; k++)
		resultPolynomial[k] = a * pol[k];

	memcpy(resPol, resultPolynomial, arraySize * 2);

	delete [] resultPolynomial;
}

// f(x) * g(x)
void polynomialMultiplication (short int *polynomial1, short int *polynomial2, short int arraySize, short int *resPol)
{
	short int *resultPolynomial = new short int [arraySize];
	short int *tempArray = new short int [arraySize];
	short int *tempArray1 = new short int [arraySize];
	
	for (short int i = 0; i < arraySize; i++)
		resultPolynomial[i] = polynomial1[0] * polynomial2[i];

	memcpy (tempArray, polynomial2, arraySize * 2);

	for (short int i = 1; i < arraySize - 1; i++)
	{
		XMultiplication (tempArray, arraySize, tempArray);

		for (short int j = 0; j < arraySize; j++)
		{
			numberMultiplication (tempArray, arraySize, polynomial1[i], tempArray1);
			resultPolynomial[j] = resultPolynomial[j] + tempArray1[j];
		}
	}

	memcpy(resPol, resultPolynomial, arraySize * 2);

	delete [] resultPolynomial;
	delete [] tempArray;
	delete [] tempArray1;
}

// x^k - x^(k-1) + ... + x^2 + 1 -> {1, 0, 1, ..., -1, 1}
void inversePolynomial( short int *pol, short int arraySize, short int modulo, char moduloPower, short int * resPol)
{
	short int k = 0;
	short int *invPol = new short int [arraySize];
	short int *c = new short int [arraySize];
	short int *f = new short int [arraySize];
	short int *g = new short int [arraySize];
	short int *temp = new short int [arraySize];

	memset (invPol, 0, arraySize * 2);
	memset (c, 0, arraySize * 2);
	memset (g, 0, arraySize * 2);
	memcpy (f, pol, arraySize * 2);
	
	invPol[0] = 1;
	g[0] = -1;
	g[arraySize - 1] = 1;

	k = 0;

	short int count = 0;
	while (count < arraySize * arraySize)
	{
		while (f[0] == 0)
		{
			XDivision (f, arraySize, f);
			XMultiplication (c, arraySize, c);
			k = k + 1;
		}

		if (polynomialDegree (f, arraySize) == 0)
		{
			numberMultiplication (invPol, arraySize, modularInverse (f[0], modulo), invPol);

			modulusReduction (invPol, arraySize, modulo, invPol);

			for (short int i = 0; i < abs (arraySize - k - 1); i++)
				XDivision (invPol, arraySize, invPol);

			short int q = modulo;

			while (q < pow((double) modulo, (double) moduloPower))
			{
				q = q * q;
				polynomialMultiplication (pol, invPol, arraySize, temp);
				numberMultiplication (temp, arraySize, -1, temp);
				temp[0] = temp[0] + 2;
				polynomialMultiplication (invPol, temp, arraySize, invPol);

				modulusReduction (invPol, arraySize, (short int) pow((double) modulo, (double) moduloPower), invPol);
			}

			memcpy(resPol, invPol, arraySize * 2);

			delete [] invPol;
			delete [] c;
			delete [] f;
			delete [] g;
			delete [] temp;

			return;
		}

		if (polynomialDegree (f, arraySize) < polynomialDegree (g, arraySize))
		{
			short int *f1 = f;
			f = g;
			g = f1;
			short int *b1 = invPol;
			invPol = c;
			c = b1;
		}

		short int u;
		u = f[0] * modularInverse (g[0], modulo);
		u = u % modulo;

		if (u < 0)
			u = modulo + u;
		
		for (short int i = 0; i < arraySize; i++)
		{
			f[i] = (f[i] - u * g[i]);
			f[i] = f[i] % modulo;
			if(f[i] < 0)
				f[i] = modulo + f[i];

			invPol[i] = (invPol[i] - u * c[i]);
			invPol[i] = invPol[i] % modulo;
			if(invPol[i] < 0)
				invPol[i] = modulo + invPol[i];
			
		}
		count++;
	}
	cout << "Polinomial is irreversible" << endl;

	delete [] invPol;
	delete [] c;
	delete [] f;
	delete [] g;
	delete [] temp;
	return;
}

//////////////////////////////////////////////////////////////////////////////////////

// f polynomial generator
void fPolynomialGenerator (short int arraySize, short int df, short int *resPol)
{
	short int *fPolynomial = new short int [arraySize];
	short int number;
	short int i;

	memset (fPolynomial, 0, arraySize * 2);

	i = 0;
	while (i < df)
	{
		number = rand() % (arraySize - 1);
		if (fPolynomial[number] == 0)
		{
			fPolynomial[number] = 1;
			i++;
		}
	}

	i = 0;
	while (i < df - 1)
	{
		number = rand() % (arraySize - 1);
		if (fPolynomial[number] == 0)
		{
			fPolynomial[number] = -1;
			i++;
		}
	}

	memcpy(resPol, fPolynomial, arraySize * 2);

	delete [] fPolynomial;
}

// g, r  polynomial generator
void grPolynomialGenerator (short int arraySize, char dgr, short int *resPol)
{
	short int *grPolynomial = new short int [arraySize];
	short int number;
	short int i;

	memset (grPolynomial, 0, arraySize * 2);

	i = 0;
	while (i < dgr)
	{
		number = rand() % (arraySize - 1);
		if (grPolynomial[number] == 0)
		{
			grPolynomial[number] = 1;
			i++;
		}
	}

	i = 0;
	while (i < dgr)
	{
		number = rand() % (arraySize - 1);
		if (grPolynomial[number] == 0)
		{
			grPolynomial[number] = -1;
			i++;
		}
	}

	memcpy(resPol, grPolynomial, arraySize * 2);

	delete [] grPolynomial;
}

// h open key generation 
void openKeyGeneration (short int *fPolynomial, short int *gPolynomial, short int arraySize, short int moduloP, short int moduloQ, char qDegree, short int *resPol)
{
	short int *hPolynomial = new short int [arraySize];

	inversePolynomial (fPolynomial, arraySize, moduloQ, qDegree, hPolynomial);
	numberMultiplication (hPolynomial, arraySize, moduloP, hPolynomial);
	polynomialMultiplication (hPolynomial, gPolynomial, arraySize, hPolynomial);

	if (qDegree < 1)
		cout << "Wrong input of exponent of modulus q; default value of 1 was used" << endl;
	if (qDegree >= 1)
		modulusReduction (hPolynomial, arraySize, (short int) pow ((double) moduloQ, (double) qDegree), hPolynomial);

	memcpy(resPol, hPolynomial, arraySize * 2);

	delete [] hPolynomial;
}

// Encryption, m - secrete polynomial
void encryption (short int *mPolynomial, short int *hPolynomial, short int arraySize, short int moduloP, short int moduloQ, char qDegree, short int dr, short int *resPol)
{
	short int *cipherPolynomial = new short int [arraySize];
	short int *rPolynomial = new short int [arraySize];
	
	grPolynomialGenerator (arraySize, dr, rPolynomial);

	for (short int k = 0; k < arraySize; k++)
	{
		mPolynomial[k] = mPolynomial[k] % moduloP;
		if (mPolynomial[k] <= (moduloP / -2))
			mPolynomial[k] = mPolynomial[k] + moduloP;
		if (mPolynomial[k] > (moduloP / 2))
			mPolynomial[k] = mPolynomial[k] - moduloP;
	}

	polynomialMultiplication (rPolynomial, hPolynomial, arraySize, cipherPolynomial);
	polynomialSum (cipherPolynomial, mPolynomial, arraySize, cipherPolynomial);

	modulusReduction (cipherPolynomial, arraySize, (short int) pow ((double) moduloQ, (double) qDegree), cipherPolynomial);

	memcpy(resPol, cipherPolynomial, arraySize * 2);

	delete [] cipherPolynomial;
	delete [] rPolynomial;
}

// Encryption, m - secrete polynomial
void encryptionLight (short int *mPolynomial, short int *fPolynomial, short int *gPolynomial, short int arraySize, short int moduloP, short int moduloQ, char qDegree, short int dr, short int *resPol)
{
	short int *cipherPolynomial = new short int [arraySize];
	short int *hPolynomial = new short int [arraySize];
	openKeyGeneration (fPolynomial, gPolynomial, arraySize, moduloP, moduloQ, qDegree, hPolynomial);
	short int *rPolynomial = new short int [arraySize];
	grPolynomialGenerator (arraySize, dr, rPolynomial);

	for (short int k = 0; k < arraySize; k++)
	{
		mPolynomial[k] = mPolynomial[k] % moduloP;
		if (mPolynomial[k] <= (moduloP / -2))
			mPolynomial[k] = mPolynomial[k] + moduloP;
		if (mPolynomial[k] > (moduloP / 2))
			mPolynomial[k] = mPolynomial[k] - moduloP;
	}

	polynomialMultiplication (rPolynomial, hPolynomial, arraySize, cipherPolynomial);
	polynomialSum (cipherPolynomial, mPolynomial, arraySize, cipherPolynomial);

	modulusReduction (cipherPolynomial, arraySize, (short int) pow ((double) moduloQ, (double) qDegree), cipherPolynomial);

	memcpy(resPol, cipherPolynomial, arraySize * 2);

	delete [] cipherPolynomial;
	delete [] hPolynomial;
	delete [] rPolynomial;
}

// Decryption
void decryption (short int *cipherPolynomial, short int *fPolynomial, short int *fpPolynomial, short int arraySize, short int moduloP, short int moduloQ, char qDegree, short int *resPol)
{
	short int *temp = new short int [arraySize];
	short int *secretePolynomial = new short int [arraySize];
	short int qPower = (short int) pow ((double) moduloQ, (double) qDegree);

	polynomialMultiplication (cipherPolynomial, fPolynomial, arraySize, temp);

	for (short int k = 0; k < arraySize; k++)
	{
		temp[k] = temp[k] % qPower;
		if (temp[k] <= (qPower / -2))
			temp[k] = temp[k] + qPower;
		if (temp[k] > (qPower / 2))
			temp[k] = temp[k] - qPower;
	}

	for (short int k = 0; k < arraySize; k++)
		secretePolynomial[k] = temp[k] % moduloP;
//	inversePolynomial (fPolynomial, arraySize, moduloP, 1, temp);
	polynomialMultiplication (secretePolynomial, fpPolynomial, arraySize, secretePolynomial);

	for (short int k = 0; k < arraySize; k++)
	{
		secretePolynomial[k] = secretePolynomial [k] % moduloP;
		if (secretePolynomial[k] <= (moduloP / -2))
			secretePolynomial[k] = secretePolynomial[k] + moduloP;
		if (secretePolynomial[k] > (moduloP / 2))
			secretePolynomial[k] = secretePolynomial[k] - moduloP;
	}

	memcpy(resPol, secretePolynomial, arraySize * 2);
	
	delete [] temp;
	delete [] secretePolynomial;
}

//////////////////////////////////////////////////////////////////////

// Message encryption
void messageEncryption (string message, short int *fPolynomial, short int *gPolynomial, short int polynomialArraySize, short int moduloP, short int moduloQ, char qDegree, 
							short int dr, short int *resPol)
{
	if (message.length () <= (polynomialArraySize - 1) / 16)
	{
		short int *M = new short int [polynomialArraySize];
		short int *encryptedPolynomial = new short int [polynomialArraySize];
		
		messageToArray (message, M);
		for (short int k = message.length () * 16; k < polynomialArraySize; k++)
			M[k] = 0;
		encryptionLight (M, fPolynomial, gPolynomial, polynomialArraySize, moduloP, moduloQ, qDegree, dr, encryptedPolynomial);
		cout << "cipher: " << endl;
		for (int k = 0; k < polynomialArraySize; k++)
			cout << encryptedPolynomial[k] << " ";
		cout << endl << "-----------------------------" << endl;
		
		memcpy(resPol, encryptedPolynomial, polynomialArraySize * 2);

		delete [] M;
		delete [] encryptedPolynomial;
	}
/*	else
	{
		int N = 16 * message.length () / (polynomialArraySize - 1);
		short int *encryptedPolynomial = new short int [N * polynomialArraySize];
		short int *A = new short int [polynomialArraySize];
		string temp;

		for (int k = 0; k <= N; k++)
		{
			temp = message.substr (k * (polynomialArraySize - 1) / 16, (polynomialArraySize - 1) / 16);
			messageToArray (temp, A);
			for (short int l = message.length () * 16; k < polynomialArraySize; k++)
				A[k] = 0;
			A = encryptionLight (A, fPolynomial, gPolynomial, polynomialArraySize, moduloP, moduloQ, qDegree, dr);
			for (short int l = 0; l < polynomialArraySize; l++)
			{
				encryptedPolynomial[l + k * polynomialArraySize] = A[l];
				cout << encryptedPolynomial[l + k * polynomialArraySize] << "  " << A[l];
			}
		}
		
		
	}*/
}

// Decryption + converting into message
string decryptionToMessage (short int *cipherPolynomial, short int *fPolynomial, short int *fpPolynomial, short int arraySize, short int moduloP, short int moduloQ, char qDegree)
{
	short int *secretePolynomial = new short int [arraySize];
	string message;

	decryption (cipherPolynomial, fPolynomial, fpPolynomial, arraySize, moduloP, moduloQ, qDegree, secretePolynomial);
	cout << "secrete polynomial: " << endl;
		for (int k = 0; k < arraySize; k++)
			cout << secretePolynomial[k] << " ";
		cout << endl << "______________________________" << endl;
	message = arrayToMessage (secretePolynomial, arraySize);

	delete [] secretePolynomial;
	return message;
}

/////////////////////////////////////////////////////////////////////


// An array of size N of short int unmbers {-1; 0; 1} or {0; 1} is an input of encryption, depending on chosen parameter p.
// Such array always must have 0 as a last elemnt.
// An output of encryption is an array of the same structure as the input array.

// Whenever run in console mode, type in a string, for instance, "Hello, World!".
// The space symbol delimits different strings, so that in the string "Hello, World!"
// words "Hello," and "World!" wil be encrypted separately.
// Be warned, that the length of any word must not exceed size (N - 1) / 2.

// Symbolic names match ones are given in wikipedia: en.wikipedia.org/wiki/NTRUEncrypt

int main()
{
	short int N =252, q = 127, p = 2, df = 35, dg = 35, dr = 22;	// NtruEncrypt parameters
	char dq = 1;
	short int * f = new short int [N];
	short int * fp = new short int [N];
	short int * g = new short int [N];
	short int * h = new short int [N];
	short int * m = new short int [N];
	short int * e = new short int [N];
	short int * d = new short int [N];

	string S, S1;
	while (true)
	{
		cin >> S;

		srand(time(0));

		fPolynomialGenerator (N, df, f);
		grPolynomialGenerator (N, dg, g);
		fPolynomialGenerator (N, df + dg, m);
		inversePolynomial (f, N, p, 1, fp);

		messageEncryption (S, f, g, N, p, q, dq, dr, e);
		S1 = decryptionToMessage (e, f, fp, N, p, q, dq);
		cout << S1 << endl;
	}
/*
	openKeyGeneration (f, g, N, p, q, dq, h);

	encryption (m, h, N, p, q, dq, dr, e);
	decryption (e, f, N, p, q, dq, d);

	for(int i = 0; i < N; i++)
		cout << d[i] - m[i] << " ";
*/	
	delete [] f;
	delete [] fp;
	delete [] g;
	delete [] h;
	delete [] m;
	delete [] e;
	delete [] d;

	return 0;
}