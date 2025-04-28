# Workshop Preparation Checklist

To get the most out of our workshop on Large Language Models, please complete the following preparation steps.

---

## 1. 📖 Read in Advance

📢 **Important Note**: This workshop will cover the fundamentals of Large Language Models (LLMs) in a condensed, three-hour format. To deepen your understanding, please review the **Additional Resources** ([here](https://github.com/FennStatistics/introductory-workshop-in-LLMs/tree/main/Additional%20Resources)) and explore some introductory materials before the workshop.

If you’d like to follow along with the code demonstrations, having a basic understanding of Python is essential. While downloading the software in advance is optional, doing so will allow you to run the code on your own device. However, **all code demonstrations will also be discussed conceptually, so you'll be able to engage fully without the software installed**.

> During the session, we’ll dive into hands-on learning, highlighting practical and impactful uses of LLMs. I look forward to exploring these applications with you!



---

## 2. 📝 Setting up your Hugging Face Inference API and Llama Models

The Inference API of Hugging Face offers access to models with significant community interest and widespread use. For a list of supported models, refer to [Hugging Face Supported Models](https://huggingface.co/docs/api-inference/supported-models).

**Precondition**: Create an Access Token by visiting [Hugging Face Tokens](https://huggingface.co/settings/tokens) and (if desired) set up a Pro account to use larger language models, such as Llama-3-70B. Additionally, you must accept the META LLAMA 3 COMMUNITY LICENSE AGREEMENT to use these models, see:

- [meta-llama/Llama-3.1-8B-Instruct](https://huggingface.co/meta-llama/Llama-3.1-8B-Instruct)
- [meta-llama/Meta-Llama-3-70B-Instruct](https://huggingface.co/meta-llama/Meta-Llama-3-70B-Instruct)

> **Remark**: Llama models operate under the **META LLAMA 3 COMMUNITY LICENSE AGREEMENT**, which grants a non-exclusive, royalty-free license for users to utilize, modify, and distribute the Llama 3 materials. This license includes requirements for attribution and naming conventions in derivative works. Note that users with over 700 million monthly active users must obtain a separate license. Meta disclaims all warranties and limits liability for the materials’ use.


**Store your keys**: In this project, **API keys are securely stored in the file `API_key.py` within the `src` (source) folder of the repository**. A template file, `API_key_adjust.py`, has been provided; to set up your API keys, copy and paste them into this file and rename it to `API_key.py`. To ensure security, avoid uploading this file to the internet, as such the `.gitignore` file is configured to prevent accidental sharing of sensitive data.

You can also create an OpenAI key, e.g., to run my replicate example, see (you pay for usage and not monthly): https://platform.openai.com/api-keys

---

## 2. 🔧 Software Setup (If Desired)

To follow along with code demonstrations during the workshop, consider installing the following software:

- **Python**: [Download here](https://www.python.org/downloads/)
  - Essential Packages:
    - Jupyter Notebook
    - Additional packages required for specific code demonstrations (see code within folders "1_fundamentals" to "6_appendix")

  - Alternative: if you prefer to avoid installing Python on your machine, consider using [Google Colab](https://colab.google/), a hosted Jupyter Notebook service that requires no setup and provides access to computing resources.

- **Anaconda**: [Download here](https://www.anaconda.com/download)

- **R and Quarto**:
  - **R**: [Download here](https://posit.co/download/rstudio-desktop/)
    - Essential Libraries:
      - Libraries needed for specific code demonstrations (see code within folders "1_fundamentals" to "6_appendix")
      - [Quarto](https://quarto.org/): An open-source scientific and technical publishing system

- **Visual Studio Code (VS Code)**: [Download here](https://code.visualstudio.com/)
  - Recommended Extensions:
    - [Python Support](https://marketplace.visualstudio.com/items?itemName=ms-python.python)
    - [Jupyter Notebook Support](https://marketplace.visualstudio.com/items?itemName=ms-toolsai.jupyter)
    - [R Programming Language Support](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r)
    - [Quarto Publishing System](https://marketplace.visualstudio.com/items?itemName=quarto.quarto)

  - **Additional Recommended Extensions for VS Code**:
    - [GitHub Copilot (AI pair programmer, subscription required)](https://marketplace.visualstudio.com/items?itemName=GitHub.copilot-chat)
    - For collaboration using GitHub (see the [GitHub Student Developer Pack](https://education.github.com/pack)):
      - [Using Git source control in VS Code](https://code.visualstudio.com/docs/sourcecontrol/overview)
      - [GitLens for enhanced Git capabilities](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens)


### 2.1 🔧🔧 Software Setup for "agentic" Retrieval Augmented Generation (If Desired)

> This code will enable automatic scanning and processing of large sets of (scientific) PDFs. **I am likely forming a working group focused on this topic with the goal of a joint publication in the future**.

You are invited to download the necessary software for this demonstration. Note that this setup requires some familiarity with software development (like containers).

1. **Set up a Self-Hosted Supabase Server**
   - Follow the instructions [here](https://supabase.com/docs/guides/self-hosting/docker) to install a self-hosted [Supabase](https://supabase.com/) server using Docker.
    - **Windows users**: To run Docker, you must first install the Windows Subsystem for Linux (WSL). Follow the WSL setup guide [here](https://learn.microsoft.com/en-us/windows/wsl/install).

2. **Process PDFs with GROBID**
   - To extract, parse, and reformat scientific PDFs, this setup uses the machine learning library **GeneRation Of BIbliographic Data (GROBID)**. This tool converts PDFs into structured XML documents and can be hosted locally via Docker. Installation instructions can be found [here](https://grobid.readthedocs.io/en/latest/Grobid-docker/#grobid-and-docker-containers).

3. **Run the Servers**
   - After installing Docker (and WSL if required), and GROBID, follow these steps to start the services:
     - **Start the local Supabase server**:
       - Open the "local supabase server" folder within the "5_summarizingLiterature" directory in VS Code.
       - In the terminal, enter:  
         ```bash
         npx supabase start
         ```
     - **Start the GROBID server**:
       - In the terminal, enter:  
         ```bash
         docker run --gpus all --init --ulimit core=0 -p 8070:8070 grobid/grobid:0.8.1
         ```

This setup will enable you to explore advanced Retrieval-Augmented Generation locally on your computer.
   

---

Complete these steps, and you’ll be ready for an engaging and productive workshop!
