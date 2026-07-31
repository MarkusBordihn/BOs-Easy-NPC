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

package de.markusbordihn.easynpc.data.objective.factory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class BuiltInObjectiveFactoryCompletenessTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  @DisplayName("Every objective type except none and custom has a registered factory")
  void testEveryObjectiveTypeHasAFactory() {
    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      if (objectiveType == ObjectiveType.NONE || objectiveType == ObjectiveType.CUSTOM) {
        assertNull(
            BuiltInObjectiveFactories.get(objectiveType),
            objectiveType + " must not have a built-in factory");
        continue;
      }

      assertNotNull(
          BuiltInObjectiveFactories.get(objectiveType),
          objectiveType + " must have a registered factory");
    }
  }

  @Test
  @DisplayName("No factory is registered for an unknown objective type")
  void testNoStrayFactoriesAreRegistered() {
    assertEquals(
        ObjectiveType.values().length - 2, BuiltInObjectiveFactories.getRegisteredTypes().size());
  }

  @Test
  @DisplayName("The resolver falls back to the custom objective registry")
  void testResolverHandlesCustomAndUnknownTypes() {
    assertNull(ObjectiveFactoryResolver.resolve(null));
    assertNull(ObjectiveFactoryResolver.resolve(new ObjectiveDataEntry(ObjectiveType.NONE)));
    assertNull(ObjectiveFactoryResolver.resolve(new ObjectiveDataEntry(ObjectiveType.CUSTOM)));
    assertNotNull(
        ObjectiveFactoryResolver.resolve(new ObjectiveDataEntry(ObjectiveType.MELEE_ATTACK)));
  }
}
