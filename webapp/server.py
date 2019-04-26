import numpy as np
from flask import Flask, request, jsonify, render_template
import pickle
import pandas as pd
import json
from sklearn.metrics.pairwise import cosine_similarity



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
        results[keyvalue].append(str(list(output.categories)[i]))
    
    return results

def content_based_user_item(input_data, city=None, state=None, zip_code=None):
    
    input_score = list(input_data.values())

    input_score = [float(i) for i in input_score]
    
    input_df = pd.DataFrame({'categories2': list(input_data.keys()),
                        'user_score': input_score})
    
    #Get dataframes
    idf_clean = pd.read_csv('idf_top20.csv')
    bus_clean = pd.read_csv('bus_top20cats.csv')
    bus_data_attr = pd.read_csv('business_info2.csv')

    #Multiply idf score and user score
    df = idf_clean.merge(input_df, on = 'categories2', how = 'left')
    df['mul_score'] = df.idf_score*df.user_score
    
    # Get list of categories with order
    top_cats = df.categories2.tolist()
    
    # Compute the score
    bus_matrix = bus_clean[top_cats].values
    weight_matrix = np.array(df.mul_score.tolist()).reshape((20,1))
    final_score = bus_matrix@weight_matrix
    
    final_score = final_score.reshape((bus_clean.shape[0],))
    
    bus_clean['score'] = final_score
    
    result = bus_clean[['business_id','score']]
    result = result.sort_values('score', ascending = False)
    
    # Map to business_attr
    result = result.merge(bus_data_attr, how = 'left', on = 'business_id')

    #Filter by city, state, and postal code
    result = result.loc[(result.postal_code == float(zip_code))&
                                    (result.city == city.lower())&
                                    (result.state == state.upper())]

    result = result[:10]

    output = {}

    for i in range(result.shape[0]):
        keyvalue = "Rank "+str(i+1) +": "+str(list(result.name)[i])
        output[keyvalue] = []
        output[keyvalue].append("Address: "+str(list(result.address)[i]))
        output[keyvalue].append("Average Rating: "+str(list(result.stars)[i]))
        output[keyvalue].append(str(list(result.categories)[i]))

    return output

def content_based_cosine(x_vec,city = None,state = None,zip_code = None):
    
    # Import business_data
    business = pd.read_csv('businessV2.csv')
    business = business.drop(['name','address','stars', 'city','state','postal_code'], axis = 1)

    business_visited = business.loc[business['business_id'].isin(x_vec)]
    
    business_filtered = business[~business['business_id'].isin(x_vec)]
  
    
    business_filtered_cat = np.matrix(business_filtered.iloc[:,11:90])
    business_visited_cat = np.matrix(business_visited.iloc[:,11:90])

    sim_mat = cosine_similarity(business_visited_cat,business_filtered_cat)
    sim_sum = list(np.mean(sim_mat, axis=0))
    
    business_filtered['similarity'] = sim_sum
    business_filtered= business_filtered[business_filtered['similarity'] > 0]
    business_filtered = business_filtered.sort_values(by=['similarity'], ascending = False)

    
    # Map to business attributes
    bus_data_attr = pd.read_csv('business_info2.csv')

    result = business_filtered.merge(bus_data_attr, how = 'left', on = 'business_id')
    #Filter by city, state, postal code
    result = result.loc[(result.postal_code == float(zip_code))&
                                    (result.city == city.lower())&
                                    (result.state == state.upper())]
    
    
    result = result[:10]

    output = {}

    for i in range(result.shape[0]):
        keyvalue = "Rank "+str(i+1) +": "+str(list(result.name)[i])
        output[keyvalue] = []
        output[keyvalue].append("Address: "+str(list(result.address)[i]))
        output[keyvalue].append("Average Rating: "+str(list(result.stars)[i]))
        output[keyvalue].append(str(list(result.categories)[i]))


    return output


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
        business = business.sample(frac = 1)
        business_list = list(zip(business.business_id, business.name))
        #TODO: Shuffle the business id list to show only 10 restaurants to select
        return render_template('new-user.html', out = business_list[:30])

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
    data = request.form.to_dict()
    bus_list = request.form.getlist('business_id')
    zipcode = data['zip_code']
    city = data['city']
    state = data['state']
    answer = data['answer']
    input_data = {k:v for k,v in data.items() if k not in ['answer','business_id','zip_code','city','state']}

    if answer.lower() == 'yes':
        output = content_based_user_item(input_data, city=city,state=state, zip_code = zipcode)

    else:
        output = content_based_cosine(bus_list, city=city, state= state, zip_code=zipcode)


    return render_template('new-user-result.html', prediction = output)



if __name__ == '__main__':
    app.run(debug = True)

