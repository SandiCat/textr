insert into user_acc (
    age, 
    gender
)
values
    (22, 'Cis Male'),
    (26, 'Cis Female');

insert into post (
    body,
    nickname,
    author__id
)
values
    ('test post, please ignore.', 'sandi', 1),
    ('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque ut ligula libero. Aenean dapibus elit sed molestie malesuada. Nam et imperdiet massa. Phasellus rutrum lacinia ante eu tincidunt. Pellentesque efficitur mattis dolor, quis hendrerit lacus hendrerit id. Pellentesque vel massa ultrices nisl varius euismod. Integer lectus nisl, eleifend et metus non, blandit hendrerit sapien. In diam eros, bibendum a augue nec, ultricies semper magna. Proin consequat ex ac orci ornare elementum. Phasellus sagittis ornare dui vitae sagittis.

Duis orci risus, efficitur non facilisis et, iaculis ut lorem. Nullam volutpat neque ac quam pharetra sodales. Vestibulum ac faucibus purus. Suspendisse id sapien velit. Nam interdum interdum orci, ut commodo sapien dictum non. Morbi imperdiet massa pharetra auctor ultrices. Donec congue bibendum massa ut finibus.

Sed at quam ullamcorper, blandit mauris a, blandit nisi. Pellentesque ac mattis tellus. Aenean congue dignissim neque, nec aliquam erat aliquam non. Phasellus commodo, lorem vitae vestibulum ullamcorper, quam urna tristique nunc, eget aliquam sapien risus ac mauris. Proin posuere mi nec erat tempus, vitae tincidunt lacus venenatis. Fusce ornare, sapien id tempus porta, ante ipsum varius ligula, a ultricies ex justo sed odio. Mauris volutpat luctus neque dignissim sollicitudin.

Praesent ornare blandit risus a rhoncus. Sed ullamcorper urna neque, a pharetra ligula dignissim nec. Duis neque sem, convallis nec massa sed, imperdiet gravida turpis. Donec in placerat tortor. Vivamus convallis, massa quis sodales molestie, metus mi interdum purus, ut gravida purus magna vitae lorem. In non pulvinar enim, ut semper eros. Quisque maximus fermentum arcu, vulputate tempor justo. Mauris eu mi eros. Interdum et malesuada fames ac ante ipsum primis in faucibus. In molestie nibh ac ante fermentum, eget condimentum arcu elementum. Cras dictum ligula molestie urna elementum, a egestas mi vulputate. Maecenas a lorem ut felis vulputate tincidunt. Aliquam eu arcu fringilla, porta mauris non, aliquam turpis. Nulla eget quam rutrum felis faucibus congue.

Ut congue, ante at imperdiet accumsan, nisl ligula convallis eros, eu ultricies tortor felis a turpis. Nulla enim elit, ullamcorper eget sagittis eget, lacinia vitae orci. Fusce cursus non quam at varius. Vestibulum nec velit sed magna dictum pulvinar. Mauris auctor, felis ac sagittis convallis, tortor orci pharetra eros, non porttitor tortor mauris sed dui. Etiam et pharetra augue. Etiam convallis nisl quis metus dictum efficitur. Sed sodales urna quis nisl hendrerit, a venenatis odio luctus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque sed suscipit erat. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi tincidunt, eros malesuada eleifend faucibus, lectus ligula cursus justo, id pretium eros libero eu sem. Sed dignissim scelerisque urna sit amet porttitor. Nunc pulvinar est et orci sollicitudin, quis lobortis mauris maximus. Nullam vulputate tincidunt diam, a efficitur massa placerat quis.'
, 'tippy', 2);
