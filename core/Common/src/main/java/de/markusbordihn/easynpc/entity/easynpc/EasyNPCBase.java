/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProgressionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttributeHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.PendingActionHandler;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Saddleable;
import net.minecraft.world.entity.SpawnGroupData;

public interface EasyNPCBase<E extends Mob>
    extends Saddleable,
        EasyNPC<E>,
        ActionEventDataCapable<E>,
        ActionHandler<E>,
        AttackDataCapable<E>,
        AttributeDataCapable<E>,
        AttributeHandler<E>,
        BaseTickHandler<E>,
        ConfigDataCapable<E>,
        ConfigurationDataCapable<E>,
        DialogDataCapable<E>,
        DisplayAttributeDataCapable<E>,
        FactionDataCapable<E>,
        ModelDataCapable<E>,
        NavigationDataCapable<E>,
        ObjectiveDataCapable<E>,
        OwnerDataCapable<E>,
        PendingActionHandler<E>,
        PresetDataCapable<E>,
        ProfessionDataCapable<E>,
        ProgressionDataCapable<E>,
        RenderDataCapable<E>,
        ServerDataCapable<E>,
        SkinDataCapable<E>,
        SoundDataCapable<E>,
        StateDataCapable<E>,
        StatusDataCapable<E>,
        TickerDataCapable<E>,
        TradingDataCapable<E>,
        VariantDataCapable<E> {

  @Override
  default <T> void setSynchedEntityData(SynchedDataIndex synchedDataIndex, T data) {
    if (synchedDataIndex.persistent) {
      StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
      if (statusData != null) {
        statusData.markNPCDataUpdated();
      }
    }
    this.setSynchedEntityData(synchedDataIndex, data, false);
  }

  default void registerEasyNPCDefaultVariant(Enum<?> variant) {
    log.debug("Register default variant for {} with variant {} ...", this, variant);
    VariantDataCapable<E> variantData = this.getEasyNPCVariantData();
    if (variantData != null) {
      if (variantData.getSkinVariantType() == variant) {
        variantData.handleSkinVariantTypeChange(variant);
      } else {
        variantData.setSkinVariantType(variant);
      }
    }
    SoundDataCapable<E> soundData = this.getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default SpawnGroupData finalizeEasyNPCSpawn(SpawnGroupData spawnGroupData) {
    log.debug("Finalize spawn for {} ...", this);

    NavigationDataCapable<?> navigationData = this.getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.setHomePositionIfMissing(this.getEntity().blockPosition());
    }

    StatusDataCapable<?> statusData = this.getEasyNPCStatusData();
    if (statusData == null || !statusData.getStatusDataFlag(StatusDataType.FINALIZED)) {
      this.registerEasyNPCDefaultData();
    } else {
      log.debug("Skip default data registration for {} ...", this);
    }

    return spawnGroupData;
  }

  default void defineEasyNPCBaseSyncedData(SynchedEntityData.Builder builder) {
    // First define variant data to ensure that all other data can be linked to the variant.
    VariantDataCapable<E> variantData = this.getEasyNPCVariantData();
    if (variantData != null) {
      variantData.defineSynchedVariantData(builder);
    }

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineSynchedActionData(builder);
    }
    AttackDataCapable<E> attackData = this.getEasyNPCAttackData();
    if (attackData != null) {
      attackData.defineSynchedAttackData(builder);
    }
    AttributeDataCapable<E> attributeData = this.getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.defineSynchedAttributeData(builder);
    }
    DialogDataCapable<E> dialogData = this.getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineSynchedDialogData(builder);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = this.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.defineSynchedDisplayAttributeData(builder);
    }
    ModelDataCapable<E> modelData = this.getEasyNPCModelData();
    if (modelData != null) {
      modelData.defineSynchedModelData(builder);
    }
    NavigationDataCapable<E> navigationData = this.getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.defineSynchedNavigationData(builder);
    }
    OwnerDataCapable<E> ownerData = this.getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.defineSynchedOwnerData(builder);
    }
    ProfessionDataCapable<E> professionData = this.getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.defineSynchedProfessionData(builder);
    }
    ProgressionDataCapable<E> progressionData = this.getEasyNPCProgressionData();
    if (progressionData != null) {
      progressionData.defineSynchedProgressionData(builder);
    }
    RenderDataCapable<E> renderData = this.getEasyNPCRenderData();
    if (renderData != null) {
      renderData.defineSynchedRenderData(builder);
    }
    SkinDataCapable<E> skinData = this.getEasyNPCSkinData();
    if (skinData != null) {
      skinData.defineSynchedSkinData(builder);
    }
    SoundDataCapable<E> soundData = this.getEasyNPCSoundData();
    if (soundData != null) {
      soundData.defineSynchedSoundData(builder);
    }
    TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.defineSynchedTradingData(builder);
    }
  }

  default void defineEasyNPCBaseServerSideData() {
    if (!this.isServerSideInstance()) {
      return;
    }

    this.getMob().setPersistenceRequired();

    ServerDataCapable<E> serverData = this.getEasyNPCServerData();
    if (serverData == null) {
      log.error("No server data available for {}", this.getEntityUUID());
      return;
    }

    if (!serverData.hasServerEntityData()) {
      serverData.defineServerEntityData();
    }

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineCustomActionData();
    }
    DialogDataCapable<E> dialogData = this.getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineCustomDialogData();
    }
    ObjectiveDataCapable<E> objectiveData = this.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.defineCustomObjectiveData();
    }
    StateDataCapable<E> stateData = this.getEasyNPCStateData();
    if (stateData != null) {
      stateData.defineCustomStateData();
    }
    PresetDataCapable<E> presetData = this.getEasyNPCPresetData();
    if (presetData != null) {
      presetData.defineCustomPresetData();
    }
    FactionDataCapable<E> factionData = this.getEasyNPCFactionData();
    if (factionData != null) {
      factionData.defineCustomFactionData();
    }
  }

  default void handleEasyNPCData(String dataName, Runnable dataOperation) {
    try {
      dataOperation.run();
    } catch (Exception exception) {
      log.error(
          "Failed to handle {} data for {} ({}), using defaults instead!",
          dataName,
          this,
          this.getEntityUUID(),
          exception);
    }
  }

  default void addEasyNPCBaseAdditionalSaveData(
      CompoundTag compoundTag, HolderLookup.Provider provider) {
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData != null) {
      this.handleEasyNPCData(
          "action event", () -> actionEventData.addAdditionalActionData(compoundTag));
    }
    AttackDataCapable<E> attackData = this.getEasyNPCAttackData();
    if (attackData != null) {
      this.handleEasyNPCData("attack", () -> attackData.addAdditionalAttackData(compoundTag));
    }
    AttributeDataCapable<E> attributeData = this.getEasyNPCAttributeData();
    if (attributeData != null) {
      this.handleEasyNPCData(
          "attribute", () -> attributeData.addAdditionalAttributeData(compoundTag));
    }
    ConfigDataCapable<E> configData = this.getEasyNPCConfigData();
    if (configData != null) {
      this.handleEasyNPCData("config", () -> configData.addAdditionalConfigData(compoundTag));
    }
    DialogDataCapable<E> dialogData = this.getEasyNPCDialogData();
    if (dialogData != null) {
      this.handleEasyNPCData("dialog", () -> dialogData.addAdditionalDialogData(compoundTag));
    }
    DisplayAttributeDataCapable<E> displayAttributeData = this.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      this.handleEasyNPCData(
          "display attribute",
          () -> displayAttributeData.addAdditionalDisplayAttributeData(compoundTag));
    }
    FactionDataCapable<E> factionData = this.getEasyNPCFactionData();
    if (factionData != null) {
      this.handleEasyNPCData("faction", () -> factionData.addAdditionalFactionData(compoundTag));
    }
    ModelDataCapable<E> modelData = this.getEasyNPCModelData();
    if (modelData != null) {
      this.handleEasyNPCData("model", () -> modelData.addAdditionalModelData(compoundTag));
    }
    NavigationDataCapable<E> navigationData = this.getEasyNPCNavigationData();
    if (navigationData != null) {
      this.handleEasyNPCData(
          "navigation", () -> navigationData.addAdditionalNavigationData(compoundTag));
    }
    ObjectiveDataCapable<E> objectiveData = this.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      this.handleEasyNPCData(
          "objective", () -> objectiveData.addAdditionalObjectiveData(compoundTag));
    }
    OwnerDataCapable<E> ownerData = this.getEasyNPCOwnerData();
    if (ownerData != null) {
      this.handleEasyNPCData("owner", () -> ownerData.addAdditionalOwnerData(compoundTag));
    }
    PresetDataCapable<E> presetData = this.getEasyNPCPresetData();
    if (presetData != null) {
      this.handleEasyNPCData("preset", () -> presetData.addAdditionalPresetData(compoundTag));
    }
    ProfessionDataCapable<E> professionData = this.getEasyNPCProfessionData();
    if (professionData != null) {
      this.handleEasyNPCData(
          "profession", () -> professionData.addAdditionalProfessionData(compoundTag));
    }
    ProgressionDataCapable<E> progressionData = this.getEasyNPCProgressionData();
    if (progressionData != null) {
      this.handleEasyNPCData(
          "progression", () -> progressionData.addAdditionalProgressionData(compoundTag));
    }
    RenderDataCapable<E> renderData = this.getEasyNPCRenderData();
    if (renderData != null) {
      this.handleEasyNPCData("render", () -> renderData.addAdditionalRenderData(compoundTag));
    }
    SkinDataCapable<E> skinData = this.getEasyNPCSkinData();
    if (skinData != null) {
      this.handleEasyNPCData("skin", () -> skinData.addAdditionalSkinData(compoundTag));
    }
    SoundDataCapable<E> soundData = this.getEasyNPCSoundData();
    if (soundData != null) {
      this.handleEasyNPCData("sound", () -> soundData.addAdditionalSoundData(compoundTag));
    }
    StateDataCapable<E> stateData = this.getEasyNPCStateData();
    if (stateData != null) {
      this.handleEasyNPCData("state", () -> stateData.addAdditionalStateData(compoundTag));
    }
    StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
    if (statusData != null) {
      this.handleEasyNPCData("status", () -> statusData.addAdditionalStatusData(compoundTag));
    }
    TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
    if (tradingData != null) {
      this.handleEasyNPCData(
          "trading", () -> tradingData.addAdditionalTradingData(compoundTag, provider));
    }
    VariantDataCapable<E> variantData = this.getEasyNPCVariantData();
    if (variantData != null) {
      this.handleEasyNPCData("variant", () -> variantData.addAdditionalVariantData(compoundTag));
    }
  }

  default void readEasyNPCBaseAdditionalSaveData(
      CompoundTag compoundTag, HolderLookup.Provider provider) {
    // First read important data to ensure that all other data can be linked to the variant.
    ConfigDataCapable<E> configData = this.getEasyNPCConfigData();
    if (configData != null) {
      this.handleEasyNPCData("config", () -> configData.readAdditionalConfigData(compoundTag));
    }
    VariantDataCapable<E> variantData = this.getEasyNPCVariantData();
    if (variantData != null) {
      this.handleEasyNPCData("variant", () -> variantData.readAdditionalVariantData(compoundTag));
    }

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData != null) {
      this.handleEasyNPCData(
          "action event", () -> actionEventData.readAdditionalActionData(compoundTag));
    }
    AttackDataCapable<E> attackData = this.getEasyNPCAttackData();
    if (attackData != null) {
      this.handleEasyNPCData("attack", () -> attackData.readAdditionalAttackData(compoundTag));
    }
    AttributeDataCapable<E> attributeData = this.getEasyNPCAttributeData();
    if (attributeData != null) {
      this.handleEasyNPCData(
          "attribute", () -> attributeData.readAdditionalAttributeData(compoundTag));
    }
    DialogDataCapable<E> dialogData = this.getEasyNPCDialogData();
    if (dialogData != null) {
      this.handleEasyNPCData("dialog", () -> dialogData.readAdditionalDialogData(compoundTag));
    }
    DisplayAttributeDataCapable<E> displayAttributeData = this.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      this.handleEasyNPCData(
          "display attribute",
          () -> displayAttributeData.readAdditionalDisplayAttributeData(compoundTag));
    }
    FactionDataCapable<E> factionData = this.getEasyNPCFactionData();
    if (factionData != null) {
      this.handleEasyNPCData("faction", () -> factionData.readAdditionalFactionData(compoundTag));
    }
    ModelDataCapable<E> modelData = this.getEasyNPCModelData();
    if (modelData != null) {
      this.handleEasyNPCData("model", () -> modelData.readAdditionalModelData(compoundTag));
    }
    NavigationDataCapable<E> navigationData = this.getEasyNPCNavigationData();
    if (navigationData != null) {
      this.handleEasyNPCData(
          "navigation", () -> navigationData.readAdditionalNavigationData(compoundTag));
    }
    OwnerDataCapable<E> ownerData = this.getEasyNPCOwnerData();
    if (ownerData != null) {
      this.handleEasyNPCData("owner", () -> ownerData.readAdditionalOwnerData(compoundTag));
    }
    PresetDataCapable<E> presetData = this.getEasyNPCPresetData();
    if (presetData != null) {
      this.handleEasyNPCData("preset", () -> presetData.readAdditionalPresetData(compoundTag));
    }
    ProfessionDataCapable<E> professionData = this.getEasyNPCProfessionData();
    if (professionData != null) {
      this.handleEasyNPCData(
          "profession", () -> professionData.readAdditionalProfessionData(compoundTag));
    }
    ProgressionDataCapable<E> progressionData = this.getEasyNPCProgressionData();
    if (progressionData != null) {
      this.handleEasyNPCData(
          "progression", () -> progressionData.readAdditionalProgressionData(compoundTag));
    }
    RenderDataCapable<E> renderData = this.getEasyNPCRenderData();
    if (renderData != null) {
      this.handleEasyNPCData("render", () -> renderData.readAdditionalRenderData(compoundTag));
    }
    SkinDataCapable<E> skinData = this.getEasyNPCSkinData();
    if (skinData != null) {
      this.handleEasyNPCData("skin", () -> skinData.readAdditionalSkinData(compoundTag));
    }
    SoundDataCapable<E> soundData = this.getEasyNPCSoundData();
    if (soundData != null) {
      this.handleEasyNPCData("sound", () -> soundData.readAdditionalSoundData(compoundTag));
    }
    StateDataCapable<E> stateData = this.getEasyNPCStateData();
    if (stateData != null) {
      this.handleEasyNPCData("state", () -> stateData.readAdditionalStateData(compoundTag));
    }
    StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
    if (statusData != null) {
      this.handleEasyNPCData("status", () -> statusData.readAdditionalStatusData(compoundTag));
    }
    TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
    if (tradingData != null) {
      this.handleEasyNPCData(
          "trading", () -> tradingData.readAdditionalTradingData(compoundTag, provider));
    }

    ObjectiveDataCapable<E> objectiveData = this.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      this.handleEasyNPCData(
          "objective", () -> objectiveData.readAdditionalObjectiveData(compoundTag));
    }

    if (navigationData != null) {
      this.handleEasyNPCData("navigation refresh", navigationData::refreshNavigation);
    }
  }
}
