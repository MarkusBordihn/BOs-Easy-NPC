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

package de.markusbordihn.easynpc.entity.easynpc.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.handler.PauseManager;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.util.EnumMap;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.storage.TagValueOutput;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class StatusDataCapableTest {

  private static final String NO_AI_TAG = "NoAI";

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static StatusDataCapable<Mob> createStatusData(long lastUpdate, long lastSaved) {
    EnumMap<StatusDataType, Long> timestamps = new EnumMap<>(StatusDataType.class);
    timestamps.put(StatusDataType.NPC_DATA_LAST_UPDATE, lastUpdate);
    timestamps.put(StatusDataType.NPC_DATA_LAST_SAVED, lastSaved);
    return createStatusData(new EnumMap<>(StatusDataType.class), timestamps);
  }

  private static StatusDataCapable<Mob> createStatusData(boolean paused) {
    EnumMap<StatusDataType, Boolean> flags = new EnumMap<>(StatusDataType.class);
    flags.put(StatusDataType.PAUSED, paused);
    return createStatusData(flags, new EnumMap<>(StatusDataType.class));
  }

  private static CompoundTag saveWithNoAi(StatusDataCapable<Mob> statusData) {
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);
    valueOutput.putBoolean(NO_AI_TAG, true);
    statusData.addAdditionalStatusData(valueOutput);
    return valueOutput.buildResult();
  }

  @SuppressWarnings("unchecked")
  private static StatusDataCapable<Mob> createStatusData(
      EnumMap<StatusDataType, Boolean> flags, EnumMap<StatusDataType, Long> timestamps) {
    return (StatusDataCapable<Mob>)
        Proxy.newProxyInstance(
            StatusDataCapable.class.getClassLoader(),
            new Class<?>[] {StatusDataCapable.class},
            (proxy, method, arguments) -> {
              if (method.getName().equals("getStatusDataFlags")) {
                return flags;
              }
              if (method.getName().equals("getStatusDataTimestamps")) {
                return timestamps;
              }
              if (method.getName().equals("isClientSideInstance")) {
                return false;
              }
              if (method.getName().equals("setSynchedEntityData")) {
                return null;
              }
              if (method.isDefault()) {
                return InvocationHandler.invokeDefault(proxy, method, arguments);
              }

              throw new UnsupportedOperationException(method.getName());
            });
  }

  @AfterEach
  void resetGlobalPause() {
    PauseManager.reset();
  }

  @Test
  @DisplayName("Does not save a global pause into an NPC which is not paused itself")
  void globalPauseIsNotSavedIntoRunningNPC() {
    PauseManager.setGlobalPause(true);

    assertFalse(saveWithNoAi(createStatusData(false)).contains(NO_AI_TAG));
  }

  @Test
  @DisplayName("Keeps NoAI of an individually paused NPC during a global pause")
  void globalPauseKeepsNoAiOfPausedNPC() {
    PauseManager.setGlobalPause(true);

    assertTrue(saveWithNoAi(createStatusData(true)).contains(NO_AI_TAG));
  }

  @Test
  @DisplayName("Keeps a manually set NoAI without a global pause")
  void keepsManualNoAiWithoutGlobalPause() {
    assertTrue(saveWithNoAi(createStatusData(false)).contains(NO_AI_TAG));
  }

  @Test
  void markSavedKeepsCleanTimestampUnchanged() {
    StatusDataCapable<Mob> statusData = createStatusData(100L, 100L);

    statusData.markNPCDataSaved();

    assertEquals(100L, statusData.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED));
    assertFalse(statusData.hasUnsavedNPCData());
  }

  @Test
  void markSavedCopiesLastUpdateTimestamp() {
    StatusDataCapable<Mob> statusData = createStatusData(200L, 100L);

    assertTrue(statusData.hasUnsavedNPCData());
    statusData.markNPCDataSaved();

    assertEquals(200L, statusData.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED));
    assertFalse(statusData.hasUnsavedNPCData());
  }

  @Test
  void markUpdatedAdvancesPastFutureSavedTimestamp() {
    long futureTimestamp = System.currentTimeMillis() + 60_000L;
    StatusDataCapable<Mob> statusData = createStatusData(futureTimestamp, futureTimestamp);

    statusData.markNPCDataUpdated();

    assertEquals(
        futureTimestamp + 1L,
        statusData.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE));
    assertTrue(statusData.hasUnsavedNPCData());
  }
}
