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

package de.markusbordihn.easynpc.condition;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.api.condition.ConditionEvaluator;
import de.markusbordihn.easynpc.api.condition.ConditionRegistry;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import java.util.function.BooleanSupplier;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CustomConditionTest {

  private static ConditionDataEntry conditionFor(String path) {
    return new ConditionDataEntry(Identifier.fromNamespaceAndPath("custom_condition_test", path));
  }

  private static ConditionEvaluator clientCapableEvaluator(BooleanSupplier clientResult) {
    return new ConditionEvaluator() {
      @Override
      public boolean evaluate(
          ConditionDataEntry entry, ServerPlayer serverPlayer, LivingEntity npcContext) {
        return true;
      }

      @Override
      public boolean evaluateOnClient(
          ConditionDataEntry entry, Player player, LivingEntity npcContext) {
        return clientResult.getAsBoolean();
      }

      @Override
      public boolean isAvailableOnClient() {
        return true;
      }
    };
  }

  @Test
  @DisplayName("An unregistered condition hides the dialog on the server")
  void testUnregisteredConditionOnServer() {
    assertFalse(CustomCondition.evaluate(conditionFor("unregistered"), null, null));
  }

  @Test
  @DisplayName("An unregistered condition does not lock the button on the client")
  void testUnregisteredConditionOnClient() {
    assertTrue(CustomCondition.evaluateOnClient(conditionFor("unregistered_client"), null, null));
  }

  @Test
  void testRegisteredConditionIsUsed() {
    ConditionDataEntry conditionDataEntry = conditionFor("registered");
    ConditionRegistry.register(
        conditionDataEntry.customConditionId(),
        (entry, serverPlayer, npcContext) -> entry == conditionDataEntry);

    assertTrue(CustomCondition.evaluate(conditionDataEntry, null, null));
  }

  @Test
  @DisplayName("A failing evaluator hides the dialog instead of breaking the interaction")
  void testFailingEvaluatorOnServer() {
    ConditionDataEntry conditionDataEntry = conditionFor("failing");
    ConditionRegistry.register(
        conditionDataEntry.customConditionId(),
        (entry, serverPlayer, npcContext) -> {
          throw new IllegalStateException("evaluator is broken");
        });

    assertFalse(CustomCondition.evaluate(conditionDataEntry, null, null));
  }

  @Test
  @DisplayName("A server only evaluator is not asked on the client")
  void testServerOnlyEvaluatorOnClient() {
    ConditionDataEntry conditionDataEntry = conditionFor("server_only");
    ConditionRegistry.register(
        conditionDataEntry.customConditionId(), (entry, serverPlayer, npcContext) -> false);

    assertTrue(CustomCondition.evaluateOnClient(conditionDataEntry, null, null));
  }

  @Test
  void testClientCapableEvaluatorIsAsked() {
    ConditionDataEntry conditionDataEntry = conditionFor("client_capable");
    ConditionRegistry.register(
        conditionDataEntry.customConditionId(), clientCapableEvaluator(() -> false));

    assertFalse(CustomCondition.evaluateOnClient(conditionDataEntry, null, null));
  }

  @Test
  @DisplayName("A failing client evaluator leaves the button available")
  void testFailingEvaluatorOnClient() {
    ConditionDataEntry conditionDataEntry = conditionFor("failing_client");
    ConditionRegistry.register(
        conditionDataEntry.customConditionId(),
        clientCapableEvaluator(
            () -> {
              throw new IllegalStateException("evaluator is broken");
            }));

    assertTrue(CustomCondition.evaluateOnClient(conditionDataEntry, null, null));
  }
}
