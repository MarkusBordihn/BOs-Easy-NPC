/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.gametest;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.function.Predicate;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestGenerator;
import net.minecraft.gametest.framework.StructureUtils;
import net.minecraft.gametest.framework.TestFunction;

public final class GameTestFunctions {

  private GameTestFunctions() {}

  public static String enabledProperty(String modId) {
    return modId + ".gametest";
  }

  public static List<TestFunction> create(
      String modId, Predicate<String> isModLoaded, Class<?>... testClasses) {
    if (!Boolean.getBoolean(enabledProperty(modId))) {
      return List.of();
    }

    List<TestFunction> testFunctions = new ArrayList<>();
    for (Class<?> testClass : testClasses) {
      if (!hasRequiredMods(testClass, isModLoaded)) {
        continue;
      }
      Arrays.stream(testClass.getDeclaredMethods())
          .sorted(Comparator.comparing(Method::getName))
          .forEach(testMethod -> testFunctions.addAll(createTestFunctions(modId, testMethod)));
    }
    return testFunctions;
  }

  private static boolean hasRequiredMods(Class<?> testClass, Predicate<String> isModLoaded) {
    EnabledIfModLoaded enabledIfModLoaded = testClass.getAnnotation(EnabledIfModLoaded.class);
    return enabledIfModLoaded == null
        || Arrays.stream(enabledIfModLoaded.value()).allMatch(isModLoaded);
  }

  @SuppressWarnings("unchecked")
  private static Collection<TestFunction> createTestFunctions(String modId, Method testMethod) {
    GameTest gameTest = testMethod.getAnnotation(GameTest.class);
    if (gameTest != null) {
      return List.of(createTestFunction(modId, testMethod, gameTest));
    }

    if (testMethod.isAnnotationPresent(GameTestGenerator.class)) {
      return (Collection<TestFunction>) invoke(testMethod);
    }

    return List.of();
  }

  private static TestFunction createTestFunction(
      String modId, Method testMethod, GameTest gameTest) {
    String className = testMethod.getDeclaringClass().getSimpleName().toLowerCase(Locale.ROOT);
    return new TestFunction(
        gameTest.batch(),
        className + "." + testMethod.getName().toLowerCase(Locale.ROOT),
        structureName(modId, gameTest.template()),
        StructureUtils.getRotationForRotationSteps(gameTest.rotationSteps()),
        gameTest.timeoutTicks(),
        gameTest.setupTicks(),
        gameTest.required(),
        gameTest.requiredSuccesses(),
        gameTest.attempts(),
        helper -> invoke(testMethod, helper));
  }

  private static String structureName(String modId, String template) {
    return template.contains(":") ? template : modId + ":" + template;
  }

  private static Object invoke(Method testMethod, Object... arguments) {
    try {
      Object testInstance = null;
      if (!Modifier.isStatic(testMethod.getModifiers())) {
        testInstance = testMethod.getDeclaringClass().getDeclaredConstructor().newInstance();
      }
      return testMethod.invoke(testInstance, arguments);
    } catch (InvocationTargetException e) {
      if (e.getCause() instanceof RuntimeException runtimeException) {
        throw runtimeException;
      }
      throw new IllegalStateException(e.getCause());
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException("Unable to invoke game test " + testMethod, e);
    }
  }
}
