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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.gametest.ServerMessageAssertions.SurvivalOwnerAccess;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeHomePositionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangePositionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeProfessionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeRendererMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeSkinMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeSoundMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveNPCMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RespawnNPCMessage;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.sound.SoundDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public final class AppearanceMessageTestHelper {

  private static final String CHANGED_PLAYER_SKIN_NAME = "TestSkinPlayer";
  private static final UUID CHANGED_PLAYER_SKIN_UUID =
      UUID.fromString("5d0f2a3c-7c1e-4b6a-9f3e-2a1b0c9d8e7f");
  private static final String CHANGED_REMOTE_SKIN_URL = "https://example.com/easy_npc_skin.png";
  private static final Profession CHANGED_PROFESSION = Profession.LIBRARIAN;
  private static final RenderType CHANGED_RENDER_TYPE = RenderType.CUSTOM_ENTITY;
  private static final EntityType<?> CHANGED_RENDER_ENTITY_TYPE = EntityType.PIG;
  private static final SoundType CHANGED_SOUND_TYPE = SoundType.TRADE;
  private static final Identifier CHANGED_SOUND =
      Identifier.withDefaultNamespace("block.note_block.bell");
  private static final float CHANGED_SOUND_VOLUME = 0.5F;
  private static final float CHANGED_SOUND_PITCH = 1.5F;
  private static final Vec3 CHANGED_POSITION = new Vec3(1.5, 2.0, 1.5);
  private static final BlockPos CHANGED_HOME_POSITION = new BlockPos(2, 1, 2);
  private static final float DAMAGED_HEALTH = 5.0F;

  private AppearanceMessageTestHelper() {}

  public static void assertPlayerSkinChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeSkinMessage(
                uuid,
                SkinDataEntry.createPlayerSkin(CHANGED_PLAYER_SKIN_NAME, CHANGED_PLAYER_SKIN_UUID)),
        ChangeSkinMessage::create,
        AppearanceMessageTestHelper::hasChangedPlayerSkin,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertRemoteSkinChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeSkinMessage(uuid, SkinDataEntry.createRemoteSkin(CHANGED_REMOTE_SKIN_URL)),
        ChangeSkinMessage::create,
        easyNPC -> CHANGED_REMOTE_SKIN_URL.equals(easyNPC.getEasyNPCSkinData().getSkinURL()),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertProfessionChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeProfessionMessage(uuid, CHANGED_PROFESSION),
        ChangeProfessionMessage::create,
        easyNPC -> easyNPC.getEasyNPCProfessionData().getProfession() == CHANGED_PROFESSION,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertRendererChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeRendererMessage(
                uuid,
                Optional.of(CHANGED_RENDER_TYPE),
                Optional.of(CHANGED_RENDER_ENTITY_TYPE),
                Optional.empty()),
        ChangeRendererMessage::create,
        AppearanceMessageTestHelper::hasChangedRenderer,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertSoundChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeSoundMessage(
                uuid,
                CHANGED_SOUND_TYPE,
                CHANGED_SOUND.toString(),
                CHANGED_SOUND_VOLUME,
                CHANGED_SOUND_PITCH,
                true),
        ChangeSoundMessage::create,
        AppearanceMessageTestHelper::hasChangedSound,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertSoundReset(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        AppearanceMessageTestHelper::addChangedSound,
        uuid -> new ChangeSoundMessage(uuid, CHANGED_SOUND_TYPE, "", 0.0F, 0.0F, false),
        ChangeSoundMessage::create,
        easyNPC -> !hasChangedSound(easyNPC),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertPositionChange(GameTestHelper helper, EntityType<?> entityType) {
    Vec3 changedPosition = helper.absoluteVec(CHANGED_POSITION);
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangePositionMessage(uuid, changedPosition),
        ChangePositionMessage::create,
        easyNPC -> changedPosition.equals(easyNPC.getEntity().position()),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertHomePositionChange(GameTestHelper helper, EntityType<?> entityType) {
    BlockPos changedHomePosition = helper.absolutePos(CHANGED_HOME_POSITION);
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeHomePositionMessage(uuid, changedHomePosition),
        ChangeHomePositionMessage::create,
        easyNPC ->
            changedHomePosition.equals(easyNPC.getEasyNPCNavigationData().getNPCHomePosition()),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertNPCRespawn(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC -> easyNPC.getLivingEntity().setHealth(DAMAGED_HEALTH),
        RespawnNPCMessage::new,
        RespawnNPCMessage::create,
        easyNPC -> isReplacedByHealedNPC(helper, easyNPC),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertNPCRemoval(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        RemoveNPCMessage::new,
        RemoveNPCMessage::create,
        easyNPC -> easyNPC.getEntity().isRemoved(),
        SurvivalOwnerAccess.GRANTED);
  }

  private static boolean hasChangedPlayerSkin(EasyNPC<?> easyNPC) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    return skinData.getSkinType() == SkinType.PLAYER_SKIN
        && CHANGED_PLAYER_SKIN_UUID.equals(skinData.getSkinUUID())
        && CHANGED_PLAYER_SKIN_NAME.equals(skinData.getSkinDataEntry().name());
  }

  private static boolean hasChangedRenderer(EasyNPC<?> easyNPC) {
    RenderDataEntry renderDataEntry = easyNPC.getEasyNPCRenderData().getRenderDataEntry();
    return renderDataEntry.renderType() == CHANGED_RENDER_TYPE
        && renderDataEntry.renderEntityType() == CHANGED_RENDER_ENTITY_TYPE;
  }

  private static boolean hasChangedSound(EasyNPC<?> easyNPC) {
    SoundDataSet soundDataSet = easyNPC.getEasyNPCSoundData().getSoundDataSet();
    if (soundDataSet == null) {
      return false;
    }

    SoundDataEntry soundDataEntry = soundDataSet.getSound(CHANGED_SOUND_TYPE);
    return soundDataEntry != null
        && soundDataEntry.getSoundEvent() != null
        && CHANGED_SOUND.equals(soundDataEntry.getSoundEvent().location())
        && soundDataEntry.getVolume() == CHANGED_SOUND_VOLUME
        && soundDataEntry.getPitch() == CHANGED_SOUND_PITCH;
  }

  private static void addChangedSound(EasyNPC<?> easyNPC) {
    SoundDataCapable<?> soundData = easyNPC.getEasyNPCSoundData();
    SoundDataSet soundDataSet = new SoundDataSet(soundData.getResolvedSoundDataSet());
    soundDataSet.addSound(
        CHANGED_SOUND_TYPE, CHANGED_SOUND, CHANGED_SOUND_VOLUME, CHANGED_SOUND_PITCH, true);
    soundData.setSoundDataSet(soundDataSet);
  }

  private static boolean isReplacedByHealedNPC(GameTestHelper helper, EasyNPC<?> easyNPC) {
    return easyNPC.getEntity().isRemoved()
        && helper.getLevel().getEntity(easyNPC.getEntityUUID()) instanceof EasyNPC<?> replacement
        && replacement != easyNPC
        && replacement.getLivingEntity().getHealth()
            == replacement.getLivingEntity().getMaxHealth();
  }
}
