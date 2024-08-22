%% @file job_match_mnesia_kb.hrl
%% @brief Defines records for the job matching application, including job, applicant,
%% match, and strict match data structures, along with their associated evaluation fields.
%% @description This header file contains the Erlang record definitions used in the
%% job_match application. These records represent jobs, applicants, and their evaluation
%% results (education, experience, skills). The evaluation-specific records are used to
%% store normalized points and evaluation scores within match and strict match records.

%% Represents the education level and fields for a job or applicant.
-record(education,
        {level = undefined,   %% Education level (e.g., Bachelor, Master)
         field1 = undefined,  %% Primary field of study
         field2 = undefined}).   %% Secondary field of study (if any)
%% Represents the evaluation of an applicant's education for a specific job.
-record(education_eval,
        {points = undefined,             %% Points scored for education
         normalized_points = undefined}).   %% Normalized points scored for education
%% Represents the years of experience and fields for a job or applicant.
-record(experience,
        {years = undefined,   %% Years of experience
         field1 = undefined,  %% Primary field of experience
         field2 = undefined}).   %% Secondary field of experience (if any)
%% Represents the evaluation of an applicant's experience for a specific job.
-record(experience_eval,
        {points = undefined,             %% Points scored for experience
         normalized_points = undefined}).   %% Normalized points scored for experience
%% Represents the technical skills required or possessed by a job or applicant.
-record(techskills,
        {skill1 = undefined,  %% Primary technical skill
         skill2 = undefined}).   %% Secondary technical skill (if any)
%% Represents the soft skills required or possessed by a job or applicant.
-record(softskills,
        {skill1 = undefined,  %% Primary soft skill
         skill2 = undefined}).   %% Secondary soft skill (if any)
%% Represents the evaluation of an applicant's skills for a specific job.
-record(skills_eval,
        {points = undefined,             %% Points scored for skills
         normalized_points = undefined}).   %% Normalized points scored for skills
%% Represents the score assigned to an applicant's match with a job.
-record(score,
        {value = undefined}).  %% Total score value
%% Represents the star rating assigned to an applicant's match with a job.
-record(stars,
        {value = undefined}).  %% Star rating value (e.g., 1-5 stars)
%% Represents a job with its associated data.
-record(job,
        {job_id,        %% Unique identifier for the job
         job_name,      %% Name of the job
         education,     %% Required education level and fields for the job (education record)
         experience,    %% Required years of experience and fields for the job (experience record)
         techskills,    %% Required technical skills for the job (techskills record)
         softskills}).     %% Required soft skills for the job (softskills record)
%% Represents an applicant with their details related to a specific job.
-record(applicant,
        {job_id,          %% Job ID that the applicant applied for
         applicant_id,    %% Unique identifier for the applicant
         applicant_name,  %% Name of the applicant
         education,       %% Applicant's education level and fields (education record)
         experience,      %% Applicant's experience years and fields (experience record)
         techskills,      %% Applicant's technical skills (techskills record)
         softskills}).       %% Applicant's soft skills (softskills record)
%% Represents the evaluation results of an applicant for a specific job (regular match).
-record(match,
        {job_id,            %% Job ID associated with the match
         applicant_id,      %% Applicant ID associated with the match
         job_name,          %% Name of the job
         applicant_name,    %% Name of the applicant
         score,             %% Total score for the match (score record)
         stars,             %% Star rating for the match (stars record)
         education_eval,    %% Education evaluation results (education_eval record)
         experience_eval,   %% Experience evaluation results (experience_eval record)
         skills_eval}).        %% Skills evaluation results (skills_eval record)
%% Represents the strict evaluation results of an applicant for a specific job.
-record(strict,
        {job_id,            %% Job ID associated with the strict match
         applicant_id,      %% Applicant ID associated with the strict match
         job_name,          %% Name of the job
         applicant_name,    %% Name of the applicant
         score,             %% Total score for the strict match (score record)
         stars,             %% Star rating for the strict match (stars record)
         education_eval,    %% Education evaluation results (education_eval record)
         experience_eval,   %% Experience evaluation results (experience_eval record)
         skills_eval}).        %% Skills evaluation results (skills_eval record)
