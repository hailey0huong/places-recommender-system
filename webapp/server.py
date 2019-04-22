import numpy as np
from flask import Flask, request, jsonify, render_template
import pickle
import pandas as pd
import json


app = Flask(__name__)

def get_top_new_n_for_user(user_id, zip_code, city, state, top_n = 10):
    model = pickle.load(open('SVDmodel.pkl','rb'))

    data_df = pd.read_csv('data_surprise.csv')
    bus_data_attr = pd.read_csv('business_info.csv')

    #Get list of restaurants that this user has not been to:
    new_df = data_df.loc[data_df.user_id != user_id]
    res_selected = list(set(list(new_df.business_id)))

    #Make prediction
    recom = {}
    recom['business_id'] = res_selected
    recom['predicted_rating'] = []
    for item_id in recom['business_id']:
        temp = (user_id, item_id, 0)
        recom['predicted_rating'].append(model.test([temp])[0][3]) 
                
    #Filter results by zipcode, city, state and return top n
    recom_df = pd.DataFrame(recom)
    recom_df = recom_df.merge(bus_data_attr, on = 'business_id', how = 'left')
    recom_df = recom_df.sort_values('predicted_rating', ascending = False)
    recom_df = recom_df.loc[(recom_df.postal_code == float(zip_code))&
                                    (recom_df.city == city.lower())&
                                    (recom_df.state == state.upper())]
    recom_df = recom_df.drop(['business_id','postal_code'], axis = 1)
    output = recom_df[:top_n]
    
    results = {}

    for i in range(output.shape[0]):
        keyvalue = "Rank "+str(i+1) +": "+str(list(output.name)[i])
        results[keyvalue] = []
        results[keyvalue].append("Address: "+str(list(output.address)[i]))
        results[keyvalue].append("Average Rating: "+str(list(output.stars)[i]))
    
    return results

@app.route('/')
@app.route('/index')
def homepage():
    return render_template('index.html')

@app.route('/user', methods = ['POST'])
def current_user_form():
    data = request.form.to_dict()
    if data['answer'].lower() == 'yes':
        return render_template('current_user.html')
    else:
        business = pd.read_csv('business_info.csv')
        business_list = list(set(business.name))
        #TODO: Shuffle the business id list to show only 10 restaurants to select
        return render_template('new-user.html', out = business_list)

@app.route('/current-user-result',methods = ['POST'])

def result():
    if request.method == 'POST':
        
        data = request.form.to_dict()
            
        userid = data['user_id']
        zipcode = data['zip_code']
        city = data['city']
        state = data['state']
        
        results = get_top_new_n_for_user(userid, zipcode, city, state)

        return render_template("result.html",prediction=results)

@app.route('/new-user-result', methods = ['POST'])
def result_new():
    data = request.form.getlist('business_name')
    
    out = 'TODO: show content based filtering results'

    return render_template('new-user-result.html', prediction = str(data))



if __name__ == '__main__':
    app.run(debug = True)

