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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.Constants;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestGenerator;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestFunction;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GameTestFunctionsTest {

  private static final IllegalStateException TEST_FAILURE = new IllegalStateException("failed");

  private static Map<String, TestFunction> createEnabledTestFunctions() {
    System.setProperty(GameTestFunctions.enabledProperty(Constants.MOD_ID), "true");
    return GameTestFunctions.create(Constants.MOD_ID, modId -> true, SampleTest.class).stream()
        .collect(Collectors.toMap(TestFunction::getTestName, Function.identity()));
  }

  @AfterEach
  void clearEnabledProperty() {
    System.clearProperty(GameTestFunctions.enabledProperty(Constants.MOD_ID));
  }

  @Test
  @DisplayName("Registers no game tests unless explicitly enabled")
  void registersNothingByDefault() {
    assertTrue(
        GameTestFunctions.create(Constants.MOD_ID, modId -> true, SampleTest.class).isEmpty());
  }

  @Test
  @DisplayName("Registers no game tests when only another mod is enabled")
  void ignoresTheFlagOfAnotherMod() {
    System.setProperty(GameTestFunctions.enabledProperty(Constants.MOD_ID), "true");

    assertTrue(GameTestFunctions.create("other_mod", modId -> true, SampleTest.class).isEmpty());
  }

  @Test
  @DisplayName("Skips test classes whose required mods are not loaded")
  void skipsTestClassesWithMissingMods() {
    System.setProperty(GameTestFunctions.enabledProperty(Constants.MOD_ID), "true");

    assertTrue(
        GameTestFunctions.create(
                Constants.MOD_ID, "required_mod"::equals, RequiresOtherModTest.class)
            .isEmpty());
    assertEquals(
        1,
        GameTestFunctions.create(
                Constants.MOD_ID,
                modId -> modId.equals("required_mod") || modId.equals("other_mod"),
                RequiresOtherModTest.class)
            .size());
  }

  @Test
  @DisplayName("Registers annotated test methods with vanilla names and their settings")
  void registersAnnotatedTestMethods() {
    TestFunction testFunction = createEnabledTestFunctions().get("sampletest.testwithsettings");

    assertEquals(Constants.MOD_ID + ":gametest.3x3x3", testFunction.getStructureName());
    assertEquals("sampleBatch", testFunction.getBatchName());
    assertEquals(1200, testFunction.getMaxTicks());
  }

  @Test
  @DisplayName("Keeps templates which already name their namespace")
  void keepsNamespacedTemplates() {
    TestFunction testFunction = createEnabledTestFunctions().get("sampletest.testwithnamespace");

    assertEquals("other_mod:gametest.1x1x1", testFunction.getStructureName());
  }

  @Test
  @DisplayName("Registers the game tests of generator methods")
  void registersGeneratedTests() {
    assertTrue(createEnabledTestFunctions().containsKey("generated.test"));
  }

  @Test
  @DisplayName("Rethrows the runtime exception of a failing test case")
  void rethrowsTestFailures() {
    TestFunction testFunction = createEnabledTestFunctions().get("sampletest.testwithfailure");

    assertSame(TEST_FAILURE, assertThrows(RuntimeException.class, () -> testFunction.run(null)));
  }

  @EnabledIfModLoaded({"required_mod", "other_mod"})
  static class RequiresOtherModTest {

    @GameTest(template = "gametest.3x3x3")
    void testWithRequiredMods(GameTestHelper helper) {}
  }

  static class SampleTest {

    @GameTest(template = "gametest.3x3x3", timeoutTicks = 1200, batch = "sampleBatch")
    void testWithSettings(GameTestHelper helper) {}

    @GameTest(template = "other_mod:gametest.1x1x1")
    void testWithNamespace(GameTestHelper helper) {}

    @GameTest(template = "gametest.3x3x3")
    void testWithFailure(GameTestHelper helper) {
      throw TEST_FAILURE;
    }

    @GameTestGenerator
    Collection<TestFunction> generatedTests() {
      return List.of(
          new TestFunction(
              "generated",
              "generated.test",
              Constants.MOD_ID + ":gametest.3x3x3",
              100,
              0L,
              true,
              helper -> {}));
    }
  }
}
