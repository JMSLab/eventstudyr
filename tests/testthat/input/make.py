import os
import glob

with open('code/make.log', 'w') as f:
    f.write('This file was created by make.py')

do_files = glob.glob('code/*.do')

stata = 'StataSE-64'

log_files = glob.glob('log/*.log')
for ff in log_files:
    os.remove(ff)

for ff in do_files:
    os.system(f'{stata} /e {ff}')

    with open('code/make.log', 'a') as f:
        f.write(f'Execution of {ff} completed')

    ff_base = os.path.basename(ff)
    ff_log = ff_base.replace('.do', '.log')

    os.rename(f'{ff_log}', f'log/{ff_log}')

