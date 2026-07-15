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

package de.markusbordihn.easynpc.configui.network.message.server;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.trading.TradingValueType;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import java.util.function.Function;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class ServerNetworkMessageRoundTripTest {

  private static <T extends NetworkMessageRecord> T roundTrip(
      T message, Function<FriendlyByteBuf, T> reader) {
    FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
    message.write(buffer);
    return reader.apply(buffer);
  }

  @ParameterizedTest
  @EnumSource(
      value = TradingValueType.class,
      names = {"RESET_TRADING_EVERY_MIN", "MAX_USES", "REWARD_EXP", "LAST_TRADING_RESET"})
  void testBasicTradingRoundTrip(TradingValueType type) {
    UUID uuid = UUID.randomUUID();
    ChangeBasicTradingMessage message = new ChangeBasicTradingMessage(uuid, type, 12);

    ChangeBasicTradingMessage loaded = roundTrip(message, ChangeBasicTradingMessage::create);

    assertEquals(uuid, loaded.uuid());
    assertEquals(type, loaded.tradingValueType());
    assertEquals(12, loaded.tradingValue());
  }

  @ParameterizedTest
  @EnumSource(TradingValueType.class)
  void testAdvancedTradingRoundTrip(TradingValueType type) {
    UUID uuid = UUID.randomUUID();
    ChangeAdvancedTradingMessage message = new ChangeAdvancedTradingMessage(uuid, 3, type, 1.5f);

    ChangeAdvancedTradingMessage loaded = roundTrip(message, ChangeAdvancedTradingMessage::create);

    assertEquals(uuid, loaded.uuid());
    assertEquals(3, loaded.tradingOfferIndex());
    assertEquals(type, loaded.tradingValueType());
    assertEquals(1.5f, loaded.tradingValue());
  }

  @Test
  void testSkinRoundTrip() {
    UUID uuid = UUID.randomUUID();
    SkinDataEntry skinDataEntry =
        new SkinDataEntry(
            "skin",
            "https://skinmc.net/skin.png",
            UUID.randomUUID(),
            SkinType.SECURE_REMOTE_URL,
            true,
            "content",
            123L);

    ChangeSkinMessage loaded =
        roundTrip(new ChangeSkinMessage(uuid, skinDataEntry), ChangeSkinMessage::create);

    assertEquals(uuid, loaded.uuid());
    assertEquals(skinDataEntry.createTag(), loaded.skinDataEntry().createTag());
  }

  @Test
  void testActionAndTradingOfferActionRoundTrip() {
    UUID uuid = UUID.randomUUID();
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.COMMAND, "/say hello"));

    ChangeActionEventMessage actionEvent =
        roundTrip(
            new ChangeActionEventMessage(uuid, ActionEventType.ON_INTERACTION, actionDataSet),
            ChangeActionEventMessage::create);
    ChangeTradingOfferActionMessage tradingAction =
        roundTrip(
            new ChangeTradingOfferActionMessage(uuid, 2, actionDataSet),
            ChangeTradingOfferActionMessage::create);

    assertEquals(uuid, actionEvent.uuid());
    assertEquals(1, actionEvent.actionDataSet().size());
    assertEquals(uuid, tradingAction.uuid());
    assertEquals(2, tradingAction.offerIndex());
    assertEquals(1, tradingAction.actionDataSet().size());
  }

  @Test
  void testResetExecutionLimitRoundTrip() {
    ExecutionId executionId = ExecutionId.action(UUID.randomUUID(), UUID.randomUUID());
    ResetExecutionLimitMessage loaded =
        roundTrip(
            new ResetExecutionLimitMessage(executionId, true), ResetExecutionLimitMessage::create);

    assertEquals(executionId, loaded.executionId());
    assertTrue(loaded.allPlayers());
  }

  @Test
  void testDialogAndObjectiveRoundTrip() {
    UUID uuid = UUID.randomUUID();
    UUID dialogId = UUID.randomUUID();
    DialogDataEntry dialogDataEntry = new DialogDataEntry("intro", "Intro", "Hello");
    ObjectiveDataEntry objective = new ObjectiveDataEntry(ObjectiveType.LOOK_AT_PLAYER, 7);

    SaveDialogMessage dialog =
        roundTrip(
            new SaveDialogMessage(uuid, dialogId, dialogDataEntry), SaveDialogMessage::create);
    RemoveDialogMessage removeDialog =
        roundTrip(new RemoveDialogMessage(uuid, dialogId), RemoveDialogMessage::create);
    AddOrUpdateObjectiveMessage objectiveMessage =
        roundTrip(
            new AddOrUpdateObjectiveMessage(uuid, objective), AddOrUpdateObjectiveMessage::create);

    assertEquals(uuid, dialog.uuid());
    assertEquals(dialogId, dialog.dialogId());
    assertEquals(dialogDataEntry.createTag(), dialog.dialogDataEntry().createTag());
    assertEquals(dialogId, removeDialog.dialogId());
    assertEquals(ObjectiveType.LOOK_AT_PLAYER, objectiveMessage.objectiveDataEntry().getType());
    assertEquals(7, objectiveMessage.objectiveDataEntry().getPriority());
  }

  @Test
  void testModelMessagesRoundTrip() {
    UUID uuid = UUID.randomUUID();

    ChangeModelPositionMessage position =
        roundTrip(
            new ChangeModelPositionMessage(
                uuid, ModelPartType.HEAD, new CustomPosition(1f, 2f, 3f)),
            ChangeModelPositionMessage::create);
    ChangeModelRotationMessage rotation =
        roundTrip(
            new ChangeModelRotationMessage(
                uuid, ModelPartType.RIGHT_ARM, new CustomRotation(4f, 5f, 6f, true)),
            ChangeModelRotationMessage::create);
    ChangeModelScaleMessage scale =
        roundTrip(
            new ChangeModelScaleMessage(
                uuid, ModelPartType.LEFT_ARM, new CustomScale(1.1f, 1.2f, 1.3f)),
            ChangeModelScaleMessage::create);
    ChangeModelVisibilityMessage visibility =
        roundTrip(
            new ChangeModelVisibilityMessage(uuid, ModelPartType.HELMET, false),
            ChangeModelVisibilityMessage::create);

    assertEquals(2f, position.position().y());
    assertEquals(5f, rotation.rotation().y());
    assertTrue(rotation.rotation().locked());
    assertEquals(1.3f, scale.scale().z());
    assertFalse(visibility.visible());
  }

  @Test
  void testPresetMessagesRoundTrip() {
    UUID uuid = UUID.randomUUID();
    Identifier id = Identifier.fromNamespaceAndPath("easy_npc", "default_preset/test");
    CompoundTag presetData = new CompoundTag();
    presetData.putString("id", "easy_npc:humanoid");
    PresetMetadata metadata =
        new PresetMetadata(
            "Guard", "Test", "1.0.0", "Tester", 1L, 2L, "desc", "easy_npc:humanoid", "STEVE");

    ImportPresetMessage importMessage =
        roundTrip(
            new ImportPresetMessage(uuid, PresetType.LOCAL, presetData, id),
            ImportPresetMessage::create);
    ExportPresetMessage exportMessage =
        roundTrip(
            new ExportPresetMessage(uuid, "guard.npc.nbt", PresetExportFormat.NBT, metadata),
            ExportPresetMessage::create);
    SpawnPresetMessage spawnMessage =
        roundTrip(
            new SpawnPresetMessage(PresetType.DEFAULT, id, true, presetData),
            SpawnPresetMessage::create);

    assertEquals(uuid, importMessage.uuid());
    assertEquals(PresetType.LOCAL, importMessage.presetType());
    assertEquals(presetData, importMessage.compoundTag());
    assertEquals(id, importMessage.resourceLocation());
    assertEquals("guard.npc.nbt", exportMessage.name());
    assertEquals(PresetExportFormat.NBT, exportMessage.exportFormat());
    assertEquals(metadata.toCompoundTag(), exportMessage.metadata().toCompoundTag());
    assertEquals(PresetType.DEFAULT, spawnMessage.presetType());
    assertTrue(spawnMessage.useOriginalData());
    assertEquals(presetData, spawnMessage.presetData());
  }
}
