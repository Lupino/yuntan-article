import requests
from urllib.parse import urlencode
import json
from time import time

host = 'http://127.0.0.1:3000'

def api(uri, query = None):
    if query:
        return "{}/api/{}/?{}".format(host, uri, urlencode(query))

    return "{}/api/{}".format(host, uri)

def preprocess(rsp):
    print(rsp.text)
    data = rsp.json()
    if data.get('err'):
        raise Exception(data['err'])
    return data

def check_equal(a, b, key = None):
    if key is None:
        if a != b:
            raise Exception("Value must Equal. except: {} but got {}".format(a, b))
    else:
        if a[key] != b[key]:
            raise Exception("Value must Equal. except: {} but got {}".format(a[key], b[key]))

def run_post(uri, data = None, query=None):
    rsp = requests.post(api(uri, query), data = data)
    return preprocess(rsp)

def run_get(uri, query = None):
    rsp = requests.get(api(uri, query))
    return preprocess(rsp)

def run_delete(uri, query = None):
    rsp = requests.delete(api(uri, query))
    return preprocess(rsp)

def create_article(title, summary = '', content = '', created_at = 0):
    return run_post('articles', {'title': title, 'summary': summary, 'content': content, 'created_at': created_at})

def get_article(id):
    return run_get('articles/{}'.format(id))

def get_articles(card = ''):
    return run_get('articles', {'card': card})

def delete_article(id):
    return run_delete('articles/{}'.format(id))

def update_article(id, title = '', summary = '', content = ''):
    return run_post('articles/{}'.format(id), {'title': title, 'summary': summary, 'content': content})

def update_cover(id, fid):
    return run_post('articles/{}/cover'.format(id), {'file_id': fid})

def delete_cover(id):
    return run_delete('articles/{}/cover'.format(id))

def update_extra(id, extra):
    return run_post('articles/{}/extra'.format(id), {'extra': json.dumps(extra)})

def delete_extra(id, extra):
    return run_delete('articles/{}/extra'.format(id), {'extra': json.dumps(extra)})

def clear_extra(id):
    return run_post('articles/{}/extra/clear'.format(id))

def get_file(key):
    return run_get('file/{}'.format(key))

def save_file(key, bucket = 'upload', extra = ''):
    return run_post('file/{}'.format(key), {'bucket': bucket, 'extra': extra})

def delete_file(key):
    return run_delete('file/{}'.format(key))

def create_tag(tag):
    return run_post('tags', {'tag': tag})

def get_tag_by_id(tid):
    return run_get('tags/{}'.format(tid))

def get_tag(tid = '', tag = ''):
    if tid:
        return run_get('tags', {'tag_id': tid})
    if tag:
        return run_get('tags', {'tag': tag})

def update_tag(tid, tag):
    return run_post('tags/{}'.format(tid), {'tag': tag})

def add_article_tag(aid, tid):
    return run_post('articles/{}/tags'.format(aid), {'tag_id': tid})

def delete_article_tag(aid, tid):
    return run_delete('articles/{}/tags'.format(aid), {'tag_id': tid})

def create_timeline(timeline, aid):
    return run_post('timeline/{}/{}'.format(timeline, aid))

def delete_timeline(timeline, aid):
    return run_delete('timeline/{}/{}'.format(timeline, aid))

def get_timeline(timeline):
    return run_get('timeline/{}'.format(timeline))

def save_timeline_meta(timeline, title='', summary=''):
    return run_post('timeline/{}/meta'.format(timeline), {'title': title, 'summary': summary})

def get_timeline_meta(timeline):
    return run_get('timeline/{}/meta'.format(timeline))

def delete_timeline_meta(timeline):
    return run_delete('timeline/{}/meta'.format(timeline))

def graphql(query, file_extra={}, article_extra = {}):
    return run_post('graphql', {
        'query': query,
        'file_extra': json.dumps(file_extra),
        'article_extra': json.dumps(article_extra)
    })


def save_alias(aid, alias):
    return run_post('articles/{}/alias'.format(aid), {'alias': alias})

def remove_alias(alias):
    return run_delete('alias/{}'.format(alias))

def check_alias(alias):
    return run_get('alias/{}'.format(alias))

def main():
    title = 'test title'
    summary = 'test summary'
    content = 'test content'

    ret = create_article(title)
    check_equal(ret['article']['title'], title)
    check_equal(ret['article']['content'], '')
    check_equal(ret['article']['summary'], '')
    check_equal(ret['article']['cover'], None)
    check_equal(ret['article']['extra'], None)
    check_equal(ret['article']['timelines'], [])
    check_equal(ret['article']['tags'], [])
    aid = ret['article']['id']
    ret1 = get_article(ret['article']['id'])
    check_equal(ret['article'], ret1['article'] ,'id')
    check_equal(ret['article'], ret1['article'] ,'title')
    check_equal(ret['article'], ret1['article'] ,'content')
    check_equal(ret['article'], ret1['article'] ,'summary')
    check_equal(ret['article'], ret1['article'] ,'cover')
    check_equal(ret['article'], ret1['article'] ,'extra')
    check_equal(ret['article'], ret1['article'] ,'timelines')
    check_equal(ret['article'], ret1['article'] ,'tags')

    update_article(aid, title = title)
    update_article(aid, summary = summary)
    update_article(aid, content = content)
    update_article(aid, title = title, summary = summary)
    update_article(aid, title = title, content = content)
    update_article(aid, title = title, summary = summary, content = content)

    file_key = 'file_key'
    # file = get_file(file_key)
    file = save_file(file_key)

    update_cover(aid, file['id'])

    ret = get_article(aid)

    check_equal(ret['article']['cover'], file, 'id')
    check_equal(ret['article']['cover'], file, 'extra')
    check_equal(ret['article']['cover'], file, 'key')
    check_equal(ret['article']['cover'], file, 'bucket')

    delete_cover(aid)
    ret = get_article(aid)
    check_equal(ret['article']['cover'], None)

    update_extra(aid, {'test_extra': 'test extra'})

    ret = get_article(aid)
    check_equal(ret['article']['extra']['test_extra'], 'test extra')

    update_extra(aid, {'test_extra': 'test extra1'})

    ret = get_article(aid)
    check_equal(ret['article']['extra']['test_extra'], 'test extra1')

    update_extra(aid, {'test_extra1': 'test abcd'})

    ret = get_article(aid)
    check_equal(ret['article']['extra']['test_extra'], 'test extra1')
    check_equal(ret['article']['extra']['test_extra1'], 'test abcd')

    delete_extra(aid, {'test_extra1': ''})

    ret = get_article(aid)
    check_equal(ret['article']['extra']['test_extra'], 'test extra1')
    check_equal(ret['article']['extra'].get('test_extra1'), None)

    clear_extra(aid)

    ret = get_article(aid)
    check_equal(ret['article']['extra'], None)

    tag = 'test tag'

    ret = create_tag(tag)
    tid  = ret['tag']['id']
    check_equal(ret['tag']['name'], tag)

    ret = get_tag(tid=tid)
    check_equal(ret['tag']['name'], tag)

    ret = get_tag(tag=tag)
    check_equal(ret['tag']['id'], tid)

    # update_tag(tid, 'test tag1')

    # ret = get_tag(tid=tid)
    # check_equal(ret['tag']['name'], 'test tag1')

    add_article_tag(aid, tid)

    ret = get_article(aid)
    check_equal(ret['article']['tags'], [tag])

    delete_article_tag(aid, tid)
    check_equal(ret['article']['tags'], [tag])

    ret = get_article(aid)
    check_equal(ret['article']['tags'], [])

    timeline = 'test_timeline'
    create_timeline(timeline, aid)

    ret = get_article(aid)
    check_equal(ret['article']['timelines'], [timeline])

    ret = get_timeline(timeline)
    check_equal(ret['total'], len(ret['articles']))

    delete_timeline(timeline, aid)

    ret = get_article(aid)
    check_equal(ret['article']['timelines'], [])

    ret = get_article(aid)
    check_equal(ret['article']['tags'], [])

    ret = get_article(aid)
    check_equal(ret['article']['extra'], None)

    save_alias(aid, 'alias')

    ret = check_alias('alias')
    check_equal(ret['id'], aid)


    ret = get_article('alias')
    check_equal(ret['article']['aliases'], ['alias'])
    check_equal(ret['article']['id'], aid)

    remove_alias('alias')

    ret = check_alias('alias')
    check_equal(ret['id'], 0)

    ret = get_article(aid)
    check_equal(ret['article']['aliases'], [])

    ret = get_articles('card')
    check_equal(len(ret['cards']), ret['total'])

    ret = get_articles()
    check_equal(len(ret['articles']), ret['total'])
    for art in ret['articles']:
        delete_article(art['id'])

    # ret = get_timeline_meta(timeline)
    # check_equal(ret['title'], None)
    # check_equal(ret['summary'], None)

    save_timeline_meta(timeline, title='title', summary='summary')
    ret = get_timeline_meta(timeline)
    check_equal(ret['title'], 'title')
    check_equal(ret['summary'], 'summary')

    delete_timeline_meta(timeline)

    ret = get_timeline_meta(timeline)
    check_equal(ret['title'], None)
    check_equal(ret['summary'], None)

    ret = graphql('{file(key: "file_key") { extra {test }}}', file_extra={'test': 'def'})
    check_equal(ret['data']['file']['extra']['test'], 'def')
    delete_file(file_key)

main()
