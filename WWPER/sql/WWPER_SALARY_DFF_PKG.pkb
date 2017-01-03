create or replace PACKAGE BODY WWPER_SALARY_DFF_PKG
AS
/**********************************************************************************************************
+==================================================================+
* TYPE               : Package Body
* INPUT Parameters   : None
* OUTPUT Parameters  : None
*
* PURPOSE            : This script creates a custom program to update ASS_ATTRIBUTE15 (Salary DFF) column of
                       per_all_assignments_f table .
* called from:       : (Concurrent Exceutable)
* History
* Version           Date                   Author                  Description
* --------------------------------------------------------------------------------------------
* 1.0            21-DEC-2015               Alok Gupta                 1. Initial code
*********************************************************************************************************//**********************************************************************************************************
  * Global Variable Declaration
  --*********************************************************************************************************/
   pv_debug_mode            VARCHAR2 (10) := 'Y';
   pv_conc_request_id_num   NUMBER        := fnd_global.conc_request_id;
   pv_user_id_num           NUMBER        := fnd_global.user_id;
   pv_login_id_num          NUMBER        := fnd_global.login_id;

/*************************************************************
 Name             :  p_log
 Purpose          :  write debug/log messages in log file
 Input Parameters :  pi_msg - Log message
 Output Parameters:  None
***************************************************************/
   PROCEDURE p_log (p_msg IN VARCHAR2)
   AS
   BEGIN
      IF pv_debug_mode = 'Y'
      THEN
         fnd_file.put_line (fnd_file.LOG, p_msg);
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (fnd_file.LOG, 'Error in p_log' || SQLERRM);
   END p_log;

/*************************************************************
 Name             : p_out
 Purpose          : Write into concurrent program Output file
 Input Parameters : pi_msg - Output message
 Output Parameters: None
 ***************************************************************/
   PROCEDURE p_out (p_msg IN VARCHAR2)
   AS
   BEGIN
      fnd_file.put_line (fnd_file.output, p_msg);
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (fnd_file.LOG, 'Error in p_out' || SQLERRM);
   END p_out;

/*************************************************************
 Name             : p_out
 Purpose          : Procedure to be called from Concurrent Program
 Input Parameters : No Parameter
 Output Parameters: errbuf  - Error message to be returned
                    retcode  - Error code to be returned
 ***************************************************************/
   PROCEDURE main (errbuf OUT VARCHAR2, retcode OUT NUMBER)
   AS
      CURSOR wwper_assignment_cur
      IS
         SELECT assignment_id, object_version_number,
                wwper_ff_pkg.Get_Average_Salary
                                              (assignment_id,
                                               SYSDATE
                                              ) eligible_salary,
                ass_attribute15,
				soft_coding_keyflex_id
           FROM per_all_assignments_f
          WHERE assignment_status_type_id = 1
            AND TRUNC (SYSDATE) BETWEEN TRUNC (effective_start_date)
                                    AND TRUNC (effective_end_date);

-- Local Variables
  -- -----------------------
      wwper_assignment_c          wwper_assignment_cur%ROWTYPE;
      l_rowcount                  NUMBER                                  := 0;
      l_row_number                NUMBER                                  := 0;
      l_eligibile_salary          VARCHAR2 (100);
      lc_dt_ud_mode               VARCHAR2 (100)                       := NULL;
      ln_assignment_id            NUMBER;
      ln_object_number            NUMBER;
-- Out Variables for Update Employee Assignment API
-- ----------------------------------------------------------------------------
      ln_soft_coding_keyflex_id   hr_soft_coding_keyflex.soft_coding_keyflex_id%TYPE;
      lc_concatenated_segments    VARCHAR2 (2000);
      ln_comment_id               per_all_assignments_f.comment_id%TYPE;
      lb_no_managers_warning      BOOLEAN;
-- -- Out Variables for Update Employee Assgment Criteria
      ld_effective_start_date     per_all_assignments_f.effective_start_date%TYPE;
      ld_effective_end_date       per_all_assignments_f.effective_end_date%TYPE;
      lb_other_manager_warning    BOOLEAN;
   BEGIN
      p_log
         ('***************Salary DFF update concurrent program***************'
         );
      p_log (LPAD (' ', 80, '*'));
      p_log (' ');
      p_log ('Processing Records');
      p_log (   RPAD ('Record No.', 15, ' ')
                   || RPAD ('Assignment ID', 20, ' ')
                   || RPAD ('Salary Before Updation', 25, ' ')
                   || RPAD ('Salary After Updation', 25, ' ')
                  );
      p_log (' ');
      OPEN wwper_assignment_cur;

      LOOP
         FETCH wwper_assignment_cur
          INTO wwper_assignment_c;
         EXIT WHEN wwper_assignment_cur%NOTFOUND;
         l_row_number := l_row_number + 1;
         lc_dt_ud_mode := 'CORRECTION';
         ln_assignment_id := wwper_assignment_c.assignment_id;
         ln_object_number := wwper_assignment_c.object_version_number;
		 ln_soft_coding_keyflex_id := wwper_assignment_c.soft_coding_keyflex_id;
         l_eligibile_salary := wwper_assignment_c.eligible_salary;
         p_log (   RPAD (l_row_number, 15, ' ')
                || RPAD (ln_assignment_id, 20, ' ')
                || RPAD (wwper_assignment_c.ass_attribute15, 25, ' ')
                || RPAD (l_eligibile_salary, 25, ' ')
               );

         BEGIN
		 
			  BEGIN
				 fnd_global.apps_initialize (fnd_profile.VALUE ('USER_ID'),
											 fnd_profile.VALUE ('RESP_ID'),
											 fnd_profile.VALUE ('RESP_APPL_ID')
											);
			  EXCEPTION
				 WHEN OTHERS
				 THEN
					fnd_file.put_line (fnd_file.LOG, 'Error in FND_GLOBAL.APPS_INITIALIZE:' || SQLERRM);
					ROLLBACK;
					RETURN;
			  END;		 
-- Update Employee Assignment
-- ---------------------------------------------
            hr_assignment_api.update_emp_asg
                      (                                -- Input data elements
-- ------------------------------
                       p_effective_date              => TO_DATE (SYSDATE),
                       p_datetrack_update_mode       => lc_dt_ud_mode,
                       p_assignment_id               => ln_assignment_id,
                       p_ass_attribute15             => l_eligibile_salary,
-- Output data elements
-- -------------------------------
                       p_object_version_number       => ln_object_number,

                       p_soft_coding_keyflex_id      => ln_soft_coding_keyflex_id,
                       p_concatenated_segments       => lc_concatenated_segments,
                       p_comment_id                  => ln_comment_id,
                       p_effective_start_date        => ld_effective_start_date,
                       p_effective_end_date          => ld_effective_end_date,
                       p_no_managers_warning         => lb_no_managers_warning,
                       p_other_manager_warning       => lb_other_manager_warning
                      );
            COMMIT;
			l_rowcount := l_rowcount+1;
         EXCEPTION
            WHEN OTHERS
            THEN
               ROLLBACK;
               DBMS_OUTPUT.put_line (SQLERRM);
               p_log ('Error in api ' || SQLERRM);
         END;
	  
      END LOOP;         
      CLOSE wwper_assignment_cur;
       
      IF l_rowcount = 0
      THEN
         p_log ('**********No data to be processed for updation**********');
         ELSE
          p_log ('Total records to be processed = ' || l_rowcount);
      END IF;
   END main;
END wwper_salary_dff_pkg;
/
SHOW ERRORS;
EXIT;