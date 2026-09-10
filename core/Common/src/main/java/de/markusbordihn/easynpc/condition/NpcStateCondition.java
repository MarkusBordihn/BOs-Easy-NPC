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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.data.state.StateValueType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NpcStateCondition {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private NpcStateCondition() {}

  public static boolean evaluate(ConditionDataEntry conditionDataEntry, LivingEntity npcContext) {
    StateDataCapable<?> stateData = resolveStateData(conditionDataEntry, npcContext);
    Identifier stateId = StateIdentifier.parse(conditionDataEntry.name());
    if (stateData == null || stateId == null) {
      if (stateId == null && ConditionWarnings.shouldReport("state:" + conditionDataEntry.name())) {
        log.warn(
            "'{}' is not a valid NPC state, every dialog and action using it stays hidden.",
            conditionDataEntry.name());
      }

      return false;
    }

    return matches(conditionDataEntry, stateData.getState(stateId));
  }

  public static StateValueType valueTypeOf(ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntry.subType() instanceof StateValueType stateValueType) {
      return stateValueType;
    }

    return conditionDataEntry.hasCustomData() ? StateValueType.TEXT : StateValueType.NUMBER;
  }

  public static boolean matches(ConditionDataEntry conditionDataEntry, StateEntry stateEntry) {
    ConditionOperationType operationType = conditionDataEntry.operationType();
    if (operationType.isExistenceOperation()) {
      return (stateEntry != null) == (operationType == ConditionOperationType.EXISTS);
    }

    boolean negated = operationType == ConditionOperationType.NOT_EQUALS;

    return switch (valueTypeOf(conditionDataEntry)) {
      case TEXT -> {
        String expectedText =
            conditionDataEntry.hasCustomData() ? conditionDataEntry.customData().trim() : "";
        boolean matches = (stateEntry != null ? stateEntry.asText() : "").equals(expectedText);
        yield negated != matches;
      }
      case FLAG -> {
        boolean matches =
            (stateEntry != null && stateEntry.asFlag()) == (conditionDataEntry.value() != 0);
        yield negated != matches;
      }
      case NUMBER ->
          operationType.evaluate(
              stateEntry != null ? stateEntry.asNumber() : 0, conditionDataEntry.value());
    };
  }

  private static StateDataCapable<?> resolveStateData(
      ConditionDataEntry conditionDataEntry, LivingEntity npcContext) {
    if (!(npcContext instanceof EasyNPC<?> easyNPC)) {
      return null;
    }

    if (!conditionDataEntry.hasTargetUUID()) {
      return easyNPC.getEasyNPCStateData();
    }

    EasyNPC<?> targetNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(
            conditionDataEntry.targetUUID(), easyNPC.getEntityServerLevel());
    if (targetNPC == null) {
      if (ConditionWarnings.shouldReport("state_target:" + conditionDataEntry.targetUUID())) {
        log.warn(
            "No NPC with the UUID {} was found, every dialog and action using its state stays hidden.",
            conditionDataEntry.targetUUID());
      }

      return null;
    }

    return targetNPC.getEasyNPCStateData();
  }
}
