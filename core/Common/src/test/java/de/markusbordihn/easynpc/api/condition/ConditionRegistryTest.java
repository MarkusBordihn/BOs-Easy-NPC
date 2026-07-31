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

package de.markusbordihn.easynpc.api.condition;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ConditionRegistryTest {

  private static final ConditionEvaluator ALWAYS_TRUE =
      (conditionDataEntry, serverPlayer, npcContext) -> true;

  private static ResourceLocation conditionId(String path) {
    return ResourceLocation.fromNamespaceAndPath("condition_registry_test", path);
  }

  @Test
  void testRegisterAndGet() {
    ResourceLocation conditionId = conditionId("register_and_get");
    ConditionRegistry.register(conditionId, ALWAYS_TRUE);

    assertTrue(ConditionRegistry.isRegistered(conditionId));
    assertSame(ALWAYS_TRUE, ConditionRegistry.get(conditionId));
  }

  @Test
  @DisplayName("The first registration of a condition wins")
  void testDuplicateRegistrationIsRejected() {
    ResourceLocation conditionId = conditionId("duplicate");
    ConditionEvaluator otherEvaluator = (conditionDataEntry, serverPlayer, npcContext) -> false;

    ConditionRegistry.register(conditionId, ALWAYS_TRUE);
    ConditionRegistry.register(conditionId, otherEvaluator);

    assertSame(ALWAYS_TRUE, ConditionRegistry.get(conditionId));
  }

  @Test
  void testUnknownAndNullConditions() {
    assertNull(ConditionRegistry.get(conditionId("unknown")));
    assertNull(ConditionRegistry.get(null));
    assertFalse(ConditionRegistry.isRegistered(null));
  }

  @Test
  void testNullRegistrationIsIgnored() {
    ConditionRegistry.register(null, ALWAYS_TRUE);
    ConditionRegistry.register(conditionId("null_evaluator"), null);

    assertFalse(ConditionRegistry.isRegistered(conditionId("null_evaluator")));
  }

  @Test
  @DisplayName("A condition is server only unless the evaluator says otherwise")
  void testIsEvaluatedOnClient() {
    ResourceLocation serverOnlyId = conditionId("server_only");
    ResourceLocation clientCapableId = conditionId("client_capable");
    ConditionRegistry.register(serverOnlyId, ALWAYS_TRUE);
    ConditionRegistry.register(
        clientCapableId,
        new ConditionEvaluator() {
          @Override
          public boolean evaluate(
              ConditionDataEntry conditionDataEntry,
              ServerPlayer serverPlayer,
              LivingEntity npcContext) {
            return true;
          }

          @Override
          public boolean isAvailableOnClient() {
            return true;
          }
        });

    assertFalse(ConditionRegistry.isEvaluatedOnClient(serverOnlyId));
    assertTrue(ConditionRegistry.isEvaluatedOnClient(clientCapableId));
    assertFalse(ConditionRegistry.isEvaluatedOnClient(conditionId("not_registered")));
    assertFalse(ConditionRegistry.isEvaluatedOnClient(null));
  }
}
