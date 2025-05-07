from django.shortcuts import render, get_object_or_404
from .models import Route, Point
from django.contrib.auth.decorators import login_required
from django.views.decorators.http import require_POST
from django.shortcuts import redirect, render
from .forms import RouteCreateForm, PointForm
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.views import LogoutView

def route_list(request):
    # Show only the current user's routes
    routes = Route.objects.filter(user=request.user) if request.user.is_authenticated else []
    return render(request, 'routes/route_list.html', {'routes': routes})

def route_detail(request, pk):
    route = get_object_or_404(Route, pk=pk)
    return render(request, 'routes/route_detail.html', {'route': route})

@login_required
def route_create(request):
    if request.method == 'POST':
        form = RouteCreateForm(request.POST)
        if form.is_valid():
            route = form.save(commit=False)
            route.user = request.user
            route.save()
            return redirect('route_edit', pk=route.pk)
    else:
        form = RouteCreateForm()
    return render(request, 'routes/route_create.html', {'form': form})

@login_required
def route_edit(request, pk):
    route = get_object_or_404(Route, pk=pk, user=request.user)
    points = route.points.order_by('order')
    if request.method == 'POST':
        form = PointForm(request.POST)
        if form.is_valid():
            point = form.save(commit=False)
            point.route = route
            point.save()
            return redirect('route_edit', pk=route.pk)
    else:
        form = PointForm()
    return render(request, 'routes/route_edit.html', {
        'route': route,
        'points': points,
        'form': form,
    })

@login_required
@require_POST
def point_delete(request, route_pk, point_pk):
    route = get_object_or_404(Route, pk=route_pk, user=request.user)
    point = get_object_or_404(Point, pk=point_pk, route=route)
    point.delete()
    return redirect('route_edit', pk=route.pk)

def register(request):
    if request.method == "POST":
        form = UserCreationForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect("login")
    else:
        form = UserCreationForm()
    return render(request, "registration/register.html", {"form": form})


class LogoutViewAllowGet(LogoutView):
    def get(self, request, *args, **kwargs):
        return self.post(request, *args, **kwargs)



