/**
 * @apiDefine ArticleResponse
 * @apiSuccess {Article} article Article object.
 * @apiSuccessExample {json} Success-Response:
 *    {
 *      "article": {
 *        "summary": "summary",
 *        "from_url_hash": "C5E4EAB3FEC5B3640E9FC7B78BE8BE77980D03E9",
 *        "extra": null,
 *        "cover": null,
 *        "content": "content",
 *        "from_url":"from_url",
 *        "created_at": 1490101871,
 *        "id": 2,
 *        "title": "title",
 *        "tags": []
 *      }
 *    }
 */

/**
 * @apiDefine ArticleListResponse
 * @apiSuccess {[Article]} article Article object.
 * @apiSuccess {Number} from Result from.
 * @apiSuccess {Number} size Result size.
 * @apiSuccess {Number} total Result count.
 * @apiSuccessExample {json} Success-Response:
 *    {
 *      "size": 10,
 *      "from": 0,
 *      "articles": [
 *        {
 *          "summary": "summary1",
 *          "from_url_hash": "C5E4EAB3FEC5B3640E9FC7B78BE8BE77980D03E9",
 *          "extra": null,
 *          "cover": null,
 *          "content": "content1",
 *          "from_url": "from_url",
 *          "created_at": 1490101871,
 *          "id": 2,
 *          "title": "title2",
 *          "tags": []
 *        },
 *        {
 *          "summary": "",
 *          "from_url_hash": "D794235DAD50CF3769BE8F1DCDD180B5CF77E34E",
 *          "extra": null,
 *          "cover": null,
 *          "content": "",
 *          "from_url": "http://xxxxxx",
 *          "created_at": 1299999,
 *          "id": 1,
 *          "title": "test",
 *          "tags": []
 *        }
 *      ],
 *      "total": 2
 *    }
 */

/**
 * @apiDefine ResultOK
 * @apiSuccess {String} result OK.
 * @apiSuccessExample {json} Success-Response:
 *     {
 *       "result": "OK"
 *     }
 */

/**
 * @apiDefine ResultError
 * @apiError {String} err Error message.
 * @apiErrorExample {json} Error-Response:
 *     {
 *       "err": "error message"
 *     }
 */
/**
 * @apiDefine TagResponse
 * @apiSuccess {json} tag Tag object.
 * @apiSuccessExample {json} Success-Response:
 *    {
 *      "tag": {
 *        "name": "test",
 *        "created_at": 1490151886,
 *        "id": 1
 *      }
 *    }
 */


/**
 * @apiDefine ArticleIDParam
 * @apiParam {Number} art_id ID of the Article.
 */

/**
 * @apiDefine FromSizeParam
 * @apiParam {Number} [from] From of the return list.
 * @apiParam {Number} [size] Size of the return list size.
 */

/**
 * @apiDefine TimelineParam
 * @apiParam {String} timeline Timeline name.
 *
 */

/**
 * @apiDefine TagIDParam
 * @apiParam {Number} tag_id ID of the Tag.
 *
 */

/**
 * @apiDefine TagNameParam
 * @apiParam {String} tag Name of the Tag.
 *
 */

/**
 * @apiDefine TagParam
 * @apiParam {Number} [tag_id] ID of the Tag.
 * @apiParam {String} [tag] Name of the Tag.
 *
 */

/**
 * @api {put} /api/upload?fileName=:fileName Upload a file.
 * @apiName UploadFile
 * @apiGroup File
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/upload?fileName=test.jpg -XPUT -d @test.jpg
 *
 * @apiParam {Number} [fileName] Original file name.
 *
 * @apiSuccess {Number} id ID of the File.
 * @apiSuccess {String} key  Key of the File.
 * @apiSuccess {String} bucket Bucket of the File.
 * @apiSuccess {json} extra Extra of the File.
 * @apiSuccess {Number} created_at Create time of the File.
 *
 * @apiSuccessExample {json} Success-Response:
 *    {
 *      "extra": {
 *        "height":null,
 *        "size":187297,
 *        "width":null,
 *        "name":"huabot.jpg",
 *        "type":"image/jpeg",
 *        "ext":"jpg"
 *      },
 *      "bucket":"upload",
 *      "key":"8B1147B139FAF354F47FFAC469E33E7EAD144D93",
 *      "created_at":1490075649,
 *      "id":4
 *    }
 */
function uploadFile() {}

/**
 * @api {post} /api/articles/ Create an article.
 * @apiName CreateArticle
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/ \
 *         -d title=title \
 *         -d summary=summary \
 *         -d content=content \
 *         -d from_url=from_url
 *
 * @apiParam {String} title Title of the Article.
 * @apiParam {String} [summary] Summary of the Article.
 * @apiParam {String} [content] Content of the Article.
 * @apiParam {String} from_url From URL of the Article.
 * @apiParam {Number} [created_at] Create time of the Article.
 *
 * @apiUse ArticleResponse
 *
 */
function createArticle() {}

/**
 * @api {post} /api/articles/:art_id/ Update article.
 * @apiName UpdateArticle
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/2/ \
 *         -d title=title \
 *         -d summary=summary \
 *         -d content=content
 *
 * @apiUse ArticleIDParam
 * @apiParam {String} [title] Title of the Article.
 * @apiParam {String} [summary] Summary of the Article.
 * @apiParam {String} [content] Content of the Article.
 *
 * @apiUse ArticleResponse
 * @apiUse ResultError
 *
 */
function updateArticle() {}

/**
 * @api {post} /api/articles/:art_id/cover Update article cover.
 * @apiName UpdateArticleCover
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/2/cover -d file_id=2
 *
 * @apiUse ArticleIDParam
 * @apiParam {Number} file_id ID of the File.
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function updateArticleCover() {}

/**
 * @api {delete} /api/articles/:art_id/cover Remove article cover.
 * @apiName RemoveArticleCover
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl -XDELETE http://article_host/api/articles/2/cover
 *
 * @apiUse ArticleIDParam
 * @apiParam {Number} file_id ID of the File.
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function removeArticleCover() {}

/**
 * @api {post} /api/articles/:art_id/extra Update article extra.
 * @apiName UpdateArticleExtra
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/2/extra \
 *         -d extra='{"test": "test"}'
 *
 * @apiUse ArticleIDParam
 * @apiParam {json} extra Extra want to update.
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function updateArticleExtra() {}

/**
 * @api {delete} /api/articles/:art_id/extra Remove article extra.
 * @apiName RemoveArticleExtra
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl -XDELETE http://article_host/api/articles/2/extra \
 *         -d extra='{"test": "test"}'
 *
 * @apiUse ArticleIDParam
 * @apiParam {json} extra Extra want to remove.
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function removeArticleExtra() {}

/**
 * @api {post} /api/articles/:art_id/extra/clear Clear article extra.
 * @apiName ClearArticleExtra
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl -XPOST http://article_host/api/articles/2/extra/clear
 *
 * @apiUse ArticleIDParam
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function clearArticleExtra() {}

/**
 * @api {delete} /api/articles/:art_id/ Remve article.
 * @apiName RemoveArticle
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl -XDELETE http://article_host/api/articles/2/
 *
 * @apiUse ArticleIDParam
 *
 * @apiUse ResultOK
 *
 */
function removeArticle() {}

/**
 * @api {get} /api/articles/:art_id/ Get article.
 * @apiName GetArticle
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/2/
 *
 * @apiUse ArticleIDParam
 *
 * @apiUse ArticleResponse
 *
 */
function getArticle() {}

/**
 * @api {get} /api/articles/?from=:from&size=:size Get article list.
 * @apiName GetAllArticle
 * @apiGroup Article
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/?from=0&size=10
 *
 * @apiUse FromSizeParam
 *
 * @apiUse ArticleListResponse
 *
 */
function getAllArticle() {}

/**
 * @api {get} /api/tags/:tag_id/ Get tag.
 * @apiName GetTag
 * @apiGroup Tag
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/tags/1/
 *
 * @apiUse TagIDParam
 *
 * @apiUse TagResponse
 * @apiUse ResultError
 *
 */
function getTag() {}

/**
 * @api {get} /api/tags/?tag=:tag Get tag by name.
 * @apiName GetTagByName
 * @apiGroup Tag
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/tags/?tag=test
 *
 * @apiUse TagNameParam
 *
 * @apiUse TagResponse
 * @apiUse ResultError
 *
 */
function getTagByName() {}

/**
 * @api {post} /api/tags/ Create a new tag.
 * @apiName CreateTag
 * @apiGroup Tag
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/tags/ -d tag=test
 *
 * @apiUse TagNameParam
 *
 * @apiUse TagResponse
 *
 */
function createTag() {}

/**
 * @api {post} /api/tags/:tag_id/ Update tag.
 * @apiName UpdateTag
 * @apiGroup Tag
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/tags/1/ -d tag=test
 *
 * @apiUse TagIDParam
 * @apiUse TagNameParam
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function updateTag() {}

/**
 * @api {post} /api/articles/:art_id/tags/ Add tag to article.
 * @apiName AddArticleTag
 * @apiGroup Tag
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/articles/2/tags/ -d tag=test
 *
 * @apiUse TagParam
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function addArticleTag() {}

/**
 * @api {delete} /api/articles/:art_id/tags/ Remove tag from article.
 * @apiName RemoveArticleTag
 * @apiGroup Tag
 *
 * @apiExample {curl} Example usage:
 *    curl -XDELETE http://article_host/api/articles/2/tags/ -d tag=test
 *
 * @apiUse TagParam
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function removeArticleTag() {}

/**
 * @api {get} /api/timeline/:timeline/?from=:from&size=:size Get timeline article list.
 * @apiName GetAllTimeline
 * @apiGroup Timeline
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/timeline/test/?from=0&size=10
 *
 * @apiUse TimelineParam
 * @apiUse FromSizeParam
 *
 * @apiUse ArticleListResponse
 *
 */
function getAllTimeline() {}

/**
 * @api {post} /api/timeline/:timeline/ Create timeline.
 * @apiName CreateTimeline
 * @apiGroup Timeline
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/timeline/test/ -d art_id=2
 *
 * @apiUse TimelineParam
 * @apiUse ArticleIDParam
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function createTimeline() {}

/**
 * @api {delete} /api/timeline/:timeline/:art_id/ Remove timeline.
 * @apiName RemoveTimeline
 * @apiGroup Timeline
 *
 * @apiExample {curl} Example usage:
 *    curl -XDELETE http://article_host/api/timeline/test/2/
 *
 * @apiUse TimelineParam
 * @apiUse ArticleIDParam
 *
 * @apiUse ResultOK
 * @apiUse ResultError
 *
 */
function removeTimeline() {}

/**
 * @api {post} /api/timeline/:timeline/meta Save timeline meta.
 * @apiName SaveTimelineMeta
 * @apiGroup Timeline
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/timeline/test/meta \
 *         -d title=title \
 *         -d summary=summary
 *
 * @apiUse TimelineParam
 * @apiParam {String} [title] Title of the Timeline.
 * @apiParam {String} [summary] Summary of the Timeline.
 *
 * @apiUse ResultOK
 *
 */
function saveTimelineMeta() {}

/**
 * @api {delete} /api/timeline/:timeline/meta Remove timeline meta.
 * @apiName RemoveTimelineMeta
 * @apiGroup Timeline
 *
 * @apiExample {curl} Example usage:
 *    curl -XDELETE http://article_host/api/timeline/test/meta
 *
 * @apiUse TimelineParam
 *
 * @apiUse ResultOK
 *
 */
function removeTimelineMeta() {}

/**
 * @api {get} /api/timeline/:timeline/meta Get timeline meta.
 * @apiName GetTimelineMeta
 * @apiGroup Timeline
 *
 * @apiExample {curl} Example usage:
 *    curl http://article_host/api/timeline/test/meta
 *
 * @apiUse TimelineParam
 *
 * @apiSuccess {String} title Title of the Timeline.
 * @apiSuccess {String} summary Summary of the Timeline.
 *
 */
function getTimelineMeta() {}
