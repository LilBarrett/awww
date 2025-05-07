from django.test import TestCase, Client
from django.contrib.auth.models import User
from rest_framework.test import APIClient
from .models import BackgroundImage, Route, Point

class ModelTests(TestCase):
    def test_create_and_relations(self):
        user = User.objects.create_user(username='foo', password='bar')
        bg = BackgroundImage.objects.create(name='b', image='backgrounds/block.jpg')
        route = Route.objects.create(name='R', user=user, background=bg)
        point = Point.objects.create(route=route, x=1.0, y=2.0, order=1)
        self.assertEqual(route.user, user)
        self.assertEqual(route.background, bg)
        self.assertEqual(point.route, route)

class WebAuthTests(TestCase):
    def setUp(self):
        self.client = Client()
        self.user = User.objects.create_user(username='foo', password='bar')

    def test_login_required_for_routes(self):
        response = self.client.get('/routes/')  # Adjust this to your routes page if different
        self.assertEqual(response.status_code, 302)  # Redirect to login

    def test_logged_in_can_see_own_routes(self):
        self.client.login(username='foo', password='bar')
        bg = BackgroundImage.objects.create(name='b', image='backgrounds/block.jpg')
        Route.objects.create(name='R', user=self.user, background=bg)
        response = self.client.get('/routes/')
        self.assertContains(response, 'R')

class APITests(TestCase):
    def setUp(self):
        self.user = User.objects.create_user(username='foo', password='bar')
        self.bg = BackgroundImage.objects.create(name='b', image='backgrounds/block.jpg')
        self.client = APIClient()

    def test_auth_required(self):
        response = self.client.get('/api/routes/')
        self.assertEqual(response.status_code, 401)

    def test_create_route(self):
        self.client.force_authenticate(user=self.user)
        response = self.client.post('/api/routes/', {'name': 'R', 'background': self.bg.id})
        self.assertEqual(response.status_code, 201)

    def test_route_list_only_own(self):
        other = User.objects.create_user(username='baz', password='qux')
        Route.objects.create(name='Other', user=other, background=self.bg)
        Route.objects.create(name='Mine', user=self.user, background=self.bg)
        self.client.force_authenticate(user=self.user)
        response = self.client.get('/api/routes/')
        self.assertContains(response, 'Mine')
        self.assertNotContains(response, 'Other')

    def test_create_point_and_invalid(self):
        self.client.force_authenticate(user=self.user)
        route = Route.objects.create(name='R', user=self.user, background=self.bg)
        # Valid
        resp = self.client.post(f'/api/routes/{route.id}/points/', {'x': 1, 'y': 2, 'order': 1})
        self.assertEqual(resp.status_code, 201)
        # Invalid (missing x)
        resp = self.client.post(f'/api/routes/{route.id}/points/', {'y': 2, 'order': 2})
        self.assertEqual(resp.status_code, 400)
