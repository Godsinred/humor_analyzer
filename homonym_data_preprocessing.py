import regex as re

def main():
    all_homs = set()

    infile = open('data/Untitled.txt', 'r')

    for line in infile.readlines():
        pair = line.split(',')
        pair = [i.lower().strip() for i in pair]
        # print(pair)

        for i in range(len(pair)-1):
            all_homs.add('ishom({}, {}).\n'.format(pair[i], pair[i+1]))

    infile.close()

    ######################################################################
    ######################################################################

    infile = open('data/Untitled1.txt', 'r')

    for line in infile.readlines():
        pair = line.split(' ')
        pair = [i.lower().strip() for i in pair]
        # print(pair)

        for i in range(0, len(pair)-1, 2):
            all_homs.add('ishom({}, {}).\n'.format(pair[i], pair[i+1]))

    infile.close()



    outfile = open('homonym_list.txt', 'w')
    for item in all_homs:
        outfile.write(item)
    outfile.close()

if __name__ == '__main__':
    main()
