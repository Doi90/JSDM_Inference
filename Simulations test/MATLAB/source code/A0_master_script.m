%%%% Master script to run model on Spartan

% Set ID variables

%% Possible ID options

beta_options = ['nh';
                'hi']

corr_options = ['fc';
                'ac']

cov_options = ['mo';
               'mu']

model_options = ['MPR';
                 'HPR';
                 'LPR';
                 'DPR';
				 'HLR'];

%% Extract IDs corresponding to command line argument indices

beta_id = beta_options(beta)

corr_id = corr_options(corr)

cov_id = cov_options(cov)

model_id = model_options(model)

% call scripts in order

model_definitions
A2_HMSC
A3_show_estimates
A4_generate_predictions
A5

% finished notification

disp('MATLAB portion finished')