import json
from openai import OpenAI
import tiktoken  # for counting tokens
import os
import numpy as np
from itertools import chain
import copy
import tempfile
from typing import Optional, List, Callable
from scipy import spatial

key_path = os.path.expanduser("~/openai/access_key.txt")

if os.path.exists(key_path):
    with open(key_path) as f:
        key = f.read().strip()
else:
    key = None

client =  OpenAI(api_key = key)

def read_file(file_id):
    return client.files.content(file_id).text

def read_jsonl_file(file_id):
    file = client.files.content(file_id).text
    tmp = tempfile.NamedTemporaryFile()
    with open(tmp.name,'w') as jsonl:
        jsonl.write(file)
    
    with open(tmp.name,'r') as jsonl:
        json_list = list(jsonl)
    out = [json.loads(j) for j in json_list]
    return out
    
    
    

class gpt_query:
    def __init__(self,
                 key = key,
                 seed = 1,
                 gpt_model:str = "gpt-4o-2024-11-20",
                 prompt:str = "", 
                 prompt_data: Optional[str|dict|list] = None, 
                 response_format = None,
                 max_tokens = 1024,
                 indent = None,
                 disclude_fields:list = [],
                 embedding_model:str="text-embedding-3-large"):
        """
        :param gpt_model: gpt model to use
        :param prompt: Input prompt
        :param prompt_data: Additional data to append to the end of prompt. Path to json file or a dictionary
        :param disclude_fields: Fields to disclude from the prompt data dictionary.
        """
        self.gpt_model = gpt_model
        self.prompt = prompt
        self.set_prompt_data(prompt_data)
        self.client = OpenAI(api_key = key)
        self.disclude_fields = disclude_fields
        self.response_format = response_format
        self.max_tokens = max_tokens
        self.latest_batch = None
        self.embedding_model = embedding_model
        self.seed = seed
        self.indent = indent

    
    def api_test(self):
        response =  client.chat.completions.create(
        model = self.gpt_model,
        seed = self.seed,
        messages = [{
            "role": "system",
            "content": [
                {
                    "type": "text",
                    "text": "hey"
                }
            ]
        }])
            
        return response
    
    def num_tokens(self, text:str|dict|list)->int:
        try:
            encoding = tiktoken.encoding_for_model(self.gpt_model)
        except:
            encoding = tiktoken.encoding_for_model('gpt-4o')
        
        if type(text) is not str:
            text = json.dumps(text, indent = self.indent)
        
        return len(encoding.encode(text))
    
    def set_prompt_data(self,prompt_data):
        """
        Read the prompt_data file if given as a file
        """
        if type(prompt_data) is str:
            with open(prompt_data) as f:
                prompt_data = json.load(f)
            
        self.prompt_data = prompt_data
        

    
    def prep_system(self):
        data_append  = ""
        
        prompt_data = copy.deepcopy(self.prompt_data)
        
        if type(prompt_data) is str:
            with open(prompt_data) as f:
                prompt_data = json.load(f)
                
        if type(prompt_data) is list:
            for f in self.disclude_fields:
                for x in prompt_data:
                    del x[f]
            data_append = json.dumps(prompt_data,indent = self.indent)
        elif type(prompt_data) is dict:
            for f in self.disclude_fields:
                for x in prompt_data.keys():
                    del prompt_data[x][f]
            data_append = json.dumps(prompt_data,indent = self.indent)

        return self.prompt  + "\n" + data_append
    
    def prep_messages(self,message:str|dict|list)->list:
        """
        :param message: Input message, either as text or dictionary/list to be dumped as json
        """
        
        sys_prompt = self.prep_system()
        
        if type(message) is not str:
            message = json.dumps(message,indent = self.indent)
        
        messages = [
            {
                "role": "system",
                "content": [
                    {
                        "type": "text",
                        "text": sys_prompt
                    }
                ]
            },
            {
                "role": "user",
                "content": [
                    {
                        "type":"text",
                        "text": message
                      }
                  ]
            }
        ]
        
        return messages
    
    def count_tokens(self,message:str|dict|list):
        messages = self.prep_messages(message)
        
        
        return self.num_tokens(json.dumps(messages,indent = self.indent))
    
    def count_tokens_bulk(self,inputs:str|dict):
        if type(inputs) is str:
            with open(inputs, encoding = 'utf-8') as f:
                inputs = json.load(f)
                
        tokens = {}
        
        if type(inputs) is dict:
            for key in inputs.keys():
                inp = inputs[key]
                tokens[key] = self.count_tokens(inp)
        return tokens
                
    
    def ask_gpt(self,message:str|dict|list)->dict:
        
        messages = self.prep_messages(message)
        
        response = self.client.chat.completions.create(
            model = self.gpt_model,
            messages = messages,
            response_format = self.response_format,
            max_tokens = self.max_tokens,
            seed = self.seed
        )
        
        output = [x.message.content for x in response.choices]
        
        if self.response_format is not None and self.response_format['type'] == 'json_schema':
            output = [json.loads(x) for x in output]
        
        return {
            "usage":{
                "total_tokens": response.usage.total_tokens,
                "prompt_tokens": response.usage.prompt_tokens,
                "completion_tokens": response.usage.completion_tokens
            },
            "output": output
        }

    def ask_gpt_bulk(self,inputs:str|dict, output:Optional[str] = None)->dict:
        if type(inputs) is str:
            with open(inputs,encoding = 'utf8') as f:
                inputs = json.load(f)
        
        out = {}
        
        for k in inputs.keys():
            out[k] = self.ask_gpt(inputs[k])
        
        if output is not None:
            with open(output) as f:
                json.dump(out,f)
        
        return out
        
    
    def batch_input(self,message,custom_id)->dict:
        messages = self.prep_messages(message)
        return {
            "custom_id":custom_id,
            "method": "POST",
            "url": "/v1/chat/completions",
            "body":{
                "model": self.gpt_model,
                "messages": messages,
                "response_format":self.response_format,
                "max_tokens": self.max_tokens,
                "seed": self.seed
            }
        }
    
    def create_batch(self,inputs:str|dict, file:Optional[str] = None):
        if type(inputs) is str:
            with open(inputs,encoding = 'utf-8') as f:
                inputs = json.load(f)
        if file is None:
            tmp = tempfile.NamedTemporaryFile()
            file = tmp.name
        
        
        with open(file,'w') as out_file:
            for k in inputs.keys():
                batch = self.batch_input(inputs[k],k)
                json.dump(batch,out_file)
                out_file.write('\n')
        
        batch_input_file = self.client.files.create(
            file = open(file, 'rb'),
            purpose = 'batch'
        )    
        
        batch = self.client.batches.create(
            input_file_id = batch_input_file.id,
            endpoint = "/v1/chat/completions",
            completion_window = '24h',
            metadata = {
                "description": "gpt.py"
            }
        )
        
        self.latest_batch = batch
        
        return batch
    
    def get_batch_info(self,batch_id):
        return  self.client.batches.retrieve(batch_id)
    
    def latest_batch_info(self):
        return self.get_batch_info(self.latest_batch.id)
    
    def get_jsonl_file(self, file_id, out_file = None):
        
        if out_file is None:
            tmp = tempfile.NamedTemporaryFile()
            out_file = tmp.name
        
        output = self.client.files.content(file_id)
        
        with open(out_file,'w') as jsonl:
                    jsonl.write(output.text)
        with open(out_file, 'r') as jsonl:
            json_list = list(jsonl)
        
        return json_list
    
    def read_batch(self,batch_id, output = None, batch_out_file = None):
        batch_check = self.client.batches.retrieve(batch_id)
        
        if batch_check.completed_at is None:
            return None
        else:
            if batch_check.output_file_id is not None:
                json_list = self.get_jsonl_file(batch_check.output_file_id,batch_out_file)
            else:
                json_list = []
                
            batch_result = {}
            
            for j in json_list:
                res = json.loads(j)
                out_message = [x['message']['content'].replace('\x00','').replace('\\u0000','') for x in res['response']['body']['choices']]
                if True:# self.response_format is not None and self.response_format['type'] == 'json_schema':
                    try:
                        out_message = [json.loads(x) for x in out_message]
                    except:
                        out_message = []
                
                out = {
                    "usage":{
                    "total_tokens": res['response']['body']['usage']['total_tokens'],
                    "prompt_tokens": res['response']['body']['usage']['prompt_tokens'],
                    "completion_tokens": res['response']['body']['usage']['completion_tokens']
                    },
                    "output": out_message
                }
                
                batch_result[res['custom_id']] = out
            
            if output is not None:
                with open(output,'w') as f:
                    json.dump(batch_result,f)
            
            return batch_result
    
    def embed_text(self,inputs:str|list|dict):
        dict_in = False
        if type(inputs) is str:
            with open(inputs,encoding = 'utf8') as f:
                inputs = json.load(f)
        if type(inputs) is dict:
            names = list(inputs.keys())
            inputs = list(inputs.values())
            dict_in = True
        
        embedding = self.client.embeddings.create(input = inputs, model = self.embedding_model)
        
        if dict_in:
            out = {names[embedding.data[i].index]:embedding.data[i].to_dict() for i in range(len(embedding.data))}
        else:
            out = [x.to_dict() for x in embedding.data]
        
        return out
    
    

def relatedness(x,y):
    return 1 - spatial.distance.cosine(x,y)

