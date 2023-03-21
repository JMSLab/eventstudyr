import os
import glob
import time

with open('make.log', 'w') as f:
    f.write('This file was created by make.py\n')
    f.write(f'Current date: {time.strftime("%Y-%m-%d -- %H:%M:%S")}\n\n')

do_files = glob.glob('code/*.do')

stata = 'StataSE-64'

log_files = glob.glob('log/*.log')
for ff in log_files:
    os.remove(ff)

for ff in do_files:
    os.system(f'{stata} /e {ff}')

    with open('make.log', 'a') as f:
        f.write(f'Execution of {ff} completed\n')

    ff_base = os.path.basename(ff)
    ff_log = ff_base.replace('.do', '.log')

    os.rename(f'{ff_log}', f'log/{ff_log}')

